package com.bee.platform.cloud.si.manufacture.service.manufacturebuy.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.enums.EnumStorage;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.BarCodeService;
import com.bee.platform.cloud.si.manufacture.service.SampleService;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuySampleService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuySampleWeightRelationService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyWeightMachineService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.SampleAssayResultService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import javax.validation.constraints.NotBlank;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;


/**
 * <p>
 * 采购取样表 服务实现类
 * </p>
 *
 * @author liliang123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class BuySampleServiceImpl extends ServiceImpl<BuySampleMapper, BuySample> implements BuySampleService {

    @Autowired
    private BuySampleMapper buySampleMapper;
    @Autowired
    private SampleAssayResultMapper sampleAssayResultMapper;
    @Autowired
    private SampleAssayResultService sampleAssayResultService;
    @Autowired
    private BuySampleWeightRelationMapper buySampleWeightRelationMapper;
    @Autowired
    private BuySampleWeightRelationService buySampleWeightRelationService;
    @Autowired
    private BuyWeightMachineService buyWeightMachineService;
    @Autowired
    private BuyWeightMachineMapper buyWeightMachineMapper;
    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;
    @Autowired
    private ConfigProductService productService;
    @Autowired
    private StorageService storageService;
    @Autowired
    private SampleService sampleService;
    @Autowired
    private BarCodeService barCodeService;
    @Autowired
    private BuyLogisticsBatchMapper buyLogisticsBatchMapper;

    private final int ZERO = 0;


    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult sureBuySampleResult(AuthPlatformUserInfo userInfo, BuySampleSureRQ sampleSureRQ) {
        String sampleCode = sampleSureRQ.getSampleCode();
        BuySample sample = buySampleMapper.selectOne(new BuySample()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setSampleCode(sampleCode)
                .setStatus(Status.TRUE.getKey())
                .setAssayStatus(EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey()));
        if (ObjectUtils.isEmpty(sample)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SMAPLE_NOT_FOUND);
        }
        sample.setModifier(userInfo.getName())
                .setModifyId(userInfo.getId())
                .setModifyTime(new Date())
                .setAssayResult(sampleSureRQ.getAssayResult())
                .setQualityExamStatus(EnumSampleRelation.QualityAssayStatus.ALREADY.getKey());
        if (buySampleMapper.updateById(sample) <= 0) {
            throw new BusinessException(ResCodeEnum.SMAPLE_SURE_FAIL, ExceptionMessageEnum.SMAPLE_UPDATE_FAILED);
        }
        // 查询同磅单id下的样品是否都已确认（即已出质检单）
        // 若都已确认，则车次样品将待确认
        // 样品id-磅单id====》查询同磅单下多个样品的确认情况
        List<BuySampleWeightRelation> list = buySampleWeightRelationMapper.selectList(new EntityWrapper<BuySampleWeightRelation>()
                .eq("sample_code", sampleCode)
                .eq("status", Status.TRUE.getKey()));
        Set<String> machineIds = list.stream().map(a -> a.getMachineId()).collect(Collectors.toSet());
        for (String machineId : machineIds) {
            // 默认该车全部样品已确认
            boolean allFlag = true;
            List<BuySampleWeightRelation> sampleWeightRelations = buySampleWeightRelationMapper.selectList(new EntityWrapper<BuySampleWeightRelation>()
                    .eq("machine_id", machineId)
                    .eq("status", Status.TRUE.getKey()));
            for (BuySampleWeightRelation buySampleWeightRelation : sampleWeightRelations) {
                BuySample i = buySampleMapper.selectOne(new BuySample()
                        .setSampleCode(buySampleWeightRelation.getSampleCode())
                        .setStatus(Status.TRUE.getKey())
                        .setAssayStatus(EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey()));
                if (ObjectUtils.isEmpty(i) || EnumSampleRelation.QualityAssayStatus.NOT.getKey().equals(i.getQualityExamStatus())) {
                    allFlag = false;
                    break;
                }
            }
            if (allFlag) {
                BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(new BuyWeightMachine()
                        .setMachineId(machineId)
                        .setStatus(Status.TRUE.getKey()));
                buyWeightMachine.setQualityExamStatus(EnumSampleRelation.QualityExamStatus.NO.getKey());
                // 磅单id--->多个样品id=====>找到最新样品的规格
                Set<String> sampleCodes = sampleWeightRelations.stream().map(a -> a.getSampleCode()).collect(Collectors.toSet());
                List<BuySample> samples = buySampleMapper.selectList(new EntityWrapper<BuySample>()
                        .in("sample_code", sampleCodes)
                        .eq("status", Status.TRUE.getKey())
                        .orderBy("assay_time", false));
                buyWeightMachine.setProductSpecId(samples.get(0).getProductSpecId());
                buyWeightMachine.setProductSpecName(samples.get(0).getProductSpecName());
                buyWeightMachineMapper.updateById(buyWeightMachine);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult sendBackBuySample(AuthPlatformUserInfo userInfo, BuySampleSureRQ sampleSureRQ) {
        String sampleCode = sampleSureRQ.getSampleCode();
        BuySample sample = buySampleMapper.selectOne(new BuySample()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setSampleCode(sampleCode)
                .setStatus(Status.TRUE.getKey())
                .setAssayStatus(EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey()));
        if (ObjectUtils.isEmpty(sample)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SMAPLE_NOT_FOUND);
        }
        // 通过样品编号找到其分析项结果,将其软删除
        // 将其软删除
        sampleAssayResultMapper.deleteDateById(sample.getSampleCode());
        // 样品状态设置成待化验状态
        sample.setModifier(userInfo.getName())
                .setModifyId(userInfo.getId())
                .setModifyTime(new Date())
                .setProductSpecId(null)
                .setProductSpecName(null)
                .setAssayStatus(EnumSampleRelation.SampleAssayStatus.PREPARE_ASSAY.getKey());
        if (buySampleMapper.updateById(sample) <= 0) {
            throw new BusinessException(ResCodeEnum.SMAPLE_ROBACK_FAIL, ExceptionMessageEnum.SMAPLE_UPDATE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 保存取样信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> saveSample(SaveSampleBuyRQ rq, AuthPlatformUserInfo userInfo) {
        BarCode barCode = barCodeService.checkCodeExist(rq.getSampleCode());
        if (barCode == null) {
            log.info("未找到对应的条形码，码：{}", rq.getSampleCode());
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_NOT_EXIST);
        } else if (Objects.equals(barCode.getUsed(), Status.TRUE.getKey())) {
            log.info("条形码已使用，码：{}", rq.getSampleCode());
            return ResponseResult.buildResponseResult(ResCodeEnum.CODE_ALREADY_USED);
        }
        BuyWeightMachine machineOne = null;
        List<String> machineIdList1 = rq.getMachineIdList();
        if (CollectionUtils.isEmpty(machineIdList1)) {
            log.error("--------请输入相应的榜单号--------");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        for (int i = 0; i < machineIdList1.size(); i++) {
            BuyWeightMachine machine = buyWeightMachineService.selectOne(new EntityWrapper<>(new BuyWeightMachine()
                    .setMachineId(machineIdList1.get(i))
                    .setStatus(1)));
            if (machine == null) {
                log.error("没有找到对应磅单，machineId：{}", machineIdList1.get(i));
                return ResponseResult.buildResponseResult(ResCodeEnum.WEIGHT_MACHINE_NOT_FOUND, machineIdList1.get(i));
            }
            if (i == 0) {
                machineOne = machine;
            }
        }
        String sampleCode = rq.getSampleCode();
        // 插入样品与磅单相关
        List<String> machineIdList = rq.getMachineIdList();
        if (!CollectionUtils.isEmpty(machineIdList)) {
            ArrayList<BuySampleWeightRelation> list = new ArrayList<>(machineIdList.size());
            for (String machineId : machineIdList) {
                list.add(new BuySampleWeightRelation().setSampleCode(sampleCode)
                        .setMachineId(machineId)
                        .setStatus(1)
                        .setCreateId(userInfo.getId())
                        .setCreator(userInfo.getName())
                        .setCreateTime(new Date()));
            }
            buySampleWeightRelationService.insertBatch(list);
        }
        // 插入样品相关
        DateFormat df = new SimpleDateFormat("yyyy年MM月dd日 HH:mm:ss");
        Assert.notNull(machineOne, "磅单不能为空");
        String formatTime = df.format(machineOne.getWeighingTime());
        BuySample sample = BeanUtils.copyProperties(rq, BuySample.class)
                .setWeightDate(formatTime.substring(0, 11))
                .setWeightTime(formatTime.substring(12).trim())

                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setAssayStatus(EnumSampleRelation.SampleAssayStatus.PREPARE_ASSAY.getKey())
                .setQualityExamStatus(EnumSampleRelation.QualityAssayStatus.NOT.getKey())
                .setStatus(1)
                .setCreateId(userInfo.getId())
                .setCreator(userInfo.getName())
                .setCreateTime(new Date());

        if (Objects.equals(rq.getNewAdd(), 1)) {
            // 如果是新增 合同业务id从磅单获取
            sample.setContractBusinessId(machineOne.getContractBusinessId());
        }
        buySampleMapper.insert(sample);
        // 修改对应磅单的推送状态
        BuyWeightMachine machine = new BuyWeightMachine().setSamplePushStatus(EnumSampleRelation.SamplePushStatus.ALREADY.getKey());
        buyWeightMachineService.update(machine, new EntityWrapper<>(new BuyWeightMachine()
                .setStatus(1))
                .in("machine_id", machineIdList1));
        // 更新条形码编号为已使用
        barCodeService.updateCodeUsed(barCode);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 完成取样
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> finishSample(FinishSampleBuyRQ rq, AuthPlatformUserInfo userInfo) {
        BuyWeightMachine weightMachine = buyWeightMachineService.selectOne(new EntityWrapper<>(new BuyWeightMachine()
                .setStatus(1)
                .setMachineId(rq.getMachineId())));
        if (ObjectUtils.isEmpty(weightMachine)) {
            log.info("磅单不存在");
            return ResponseResult.buildResponseResult(ResCodeEnum.WEIGHT_MACHINE_NOT_FOUND);
        }
        buyWeightMachineService.updateById(weightMachine.setSampleStatus(EnumSampleRelation.SampleStatus.COMPLETED.getKey()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * 化验人员弃用样品
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> abandonSample(SampleAssayAbandonRQ rq, AuthPlatformUserInfo userInfo) {
        BuySample sample = this.selectOne(new EntityWrapper<>(new BuySample().setSampleCode(rq.getSampleCode()).setStatus(1)));
        if (ObjectUtils.isEmpty(sample)) {
            log.info("未找到采购样品--样品编号SampleCode：{}", rq.getSampleCode());
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
        sample.setAssayStatus(EnumSampleRelation.SampleAssayStatus.ABANDON.getKey())
                .setAbandonReason(rq.getAbandonReason())
                .setAbandonPerson(userInfo.getName())
                .setAbandonId(userInfo.getId())
                .setAbandonTime(new Date())
                .setModifyId(userInfo.getId())
                .setModifier(userInfo.getName())
                .setModifyTime(new Date());
        this.updateById(sample);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 化验人员开始化验样品
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> startAssaySample(SampleAssayStartRQ rq, Object o, AuthPlatformUserInfo userInfo) {
        @NotBlank String sampleCode = rq.getSampleCode();
        if (o == null) {
            log.info("未找到采购样品--样品编号SampleCode：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
        BuySample sample = (BuySample) o;
        sample.setAssayStatus(EnumSampleRelation.SampleAssayStatus.ASSAYING.getKey())
                .setModifyId(userInfo.getId())
                .setModifier(userInfo.getName())
                .setModifyTime(new Date());
        this.updateById(sample);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult sureCarSampleResult(AuthPlatformUserInfo userInfo,
                                              BuyCarSampleSureRQ carSampleSureRQ) {
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(new BuyWeightMachine()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setMachineId(carSampleSureRQ.getMachineId())
                .setQualityExamStatus(EnumSampleRelation.QualityExamStatus.NO.getKey())
                .setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(buyWeightMachine)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.CARSAMPLE_NOT_FOUND);
        }
        buyWeightMachine.setQualityExamStatus(EnumSampleRelation.QualityExamStatus.YES.getKey());
        buyWeightMachine.setAssayResult(carSampleSureRQ.getAssayResult());
        buyWeightMachine.setProductSpecId(carSampleSureRQ.getProductSpecId());
        buyWeightMachine.setProductSpecName(carSampleSureRQ.getProductSpecName());
        // 新增质检扣重
        if(!ObjectUtils.isEmpty(carSampleSureRQ.getCarDeductWeight())) {
        	buyWeightMachine.setCarDeductWeight(carSampleSureRQ.getCarDeductWeight());
        }
        // 当合格时，默认为确认入库方式，其处理方式为确认入库
        if (EnumSampleRelation.QualityAssayResult.YES.getKey().equals(carSampleSureRQ.getAssayResult())) {
            buyWeightMachine.setHandleType(EnumStorage.ProcessMode.confirm_storage.getKey());
        }
        buyWeightMachineMapper.updateById(buyWeightMachine);
        if (carSampleSureRQ.getAssayResult().equals(EnumStorage.AnalysisResult.qualified.getKey())) {
            //质检合格生成待入库产品数据
            ConfigProduct configProduct = productService.selectById(buyWeightMachine.getProductId());
            //实际到厂时间
            Date arrivalTime1 = buyWeightMachine.getArrivalTime();
            String arrivalTime = null;
            if (arrivalTime1 != null) {
                arrivalTime = DateUtils.date2Format(null, "yyyy年MM月dd日 HH:mm:ss", arrivalTime1);
            }
            Integer productSpecId = buyWeightMachine.getProductSpecId();
            String productSpecName = buyWeightMachine.getProductSpecName();
            ProductPendingStorageDTO productPendingStorageDTO = new ProductPendingStorageDTO();
            productPendingStorageDTO.setContractId(buyWeightMachine.getContractNum())
                    .setMachineId(buyWeightMachine.getMachineId()).setLicensePlateNumber(buyWeightMachine.getTrainNumber())
                    .setProductId(buyWeightMachine.getProductId()).setProductNumber(buyWeightMachine.getNetWeight())
                    .setProductName(buyWeightMachine.getProductName()).setProductUnit(configProduct.getUnitValue())
                    .setAnalysisResult(EnumStorage.AnalysisResult.qualified.getKey())
                    .setArrivalTime(arrivalTime).setProcessMode(EnumStorage.ProcessMode.confirm_storage.getKey())
                    .setProductSpecId(productSpecId).setProductSpecName(productSpecName);
            storageService.savePendingStorageProduct(productPendingStorageDTO, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<BuyCarSampleInfoDTO> getCarSampleInfoList(AuthPlatformUserInfo userInfo, Integer confirmStatus, Pagination pagination) {
        BuyCarSampleInfoDTO dto = new BuyCarSampleInfoDTO();
        List<BuyCarSimpleDTO> buyCarSimpleDTOs = new ArrayList<>();
        List<BuyWeightMachine> machines = new ArrayList<BuyWeightMachine>();
        // 未确认
        if (EnumSampleRelation.QualityExamStatus.NO.getKey().equals(confirmStatus)) {
            machines = buyWeightMachineMapper.selectPage(pagination, new EntityWrapper<BuyWeightMachine>()
                    .eq("enterprise_id", userInfo.getOrgId())
                    .eq("factory_id", userInfo.getFactoryId())
                    .eq("quality_exam_status", EnumSampleRelation.QualityExamStatus.NO.getKey())
                    .eq("status", Status.TRUE.getKey())
                    .orderBy("weighing_time", false));
        } else {
            machines = buyWeightMachineMapper.selectPage(pagination, new EntityWrapper<BuyWeightMachine>()
                    .eq("enterprise_id", userInfo.getOrgId())
                    .eq("factory_id", userInfo.getFactoryId())
                    .eq("quality_exam_status", EnumSampleRelation.QualityExamStatus.YES.getKey())
                    .eq("status", Status.TRUE.getKey())
                    .orderBy("weighing_time", false));
        }
        if (CollectionUtils.isEmpty(machines)) {
            dto.setCounts(ZERO);
            dto.setCarSimpleDTOs(buyCarSimpleDTOs);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        buyCarSimpleDTOs = BeanUtils.assemble(BuyCarSimpleDTO.class, machines);
        dto.setCounts(buyCarSimpleDTOs.size());
        dto.setCarSimpleDTOs(buyCarSimpleDTOs);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
    }


    @Override
    public ResponseResult<BuySampleInfoDTO> getBuySampleInfoList(AuthPlatformUserInfo userInfo, Integer confirmStatus, Pagination pagination) {
        BuySampleInfoDTO dto = new BuySampleInfoDTO();
        List<SampleResultDTO> sampleResultDTOs = new ArrayList<SampleResultDTO>();
        List<BuySample> samples = new ArrayList<BuySample>();
        // 未确认-未出质检单
        if (EnumSampleRelation.QualityAssayStatus.NOT.getKey().equals(confirmStatus)) {
            samples = buySampleMapper.selectPage(pagination, new EntityWrapper<BuySample>()
                    .eq("enterprise_id", userInfo.getOrgId())
                    .eq("factory_id", userInfo.getFactoryId())
                    .eq("quality_exam_status", EnumSampleRelation.QualityAssayStatus.NOT.getKey())
                    .eq("assay_status", EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey())
                    .eq("status", Status.TRUE.getKey())
                    .orderBy("assay_time", false));
        } else {
            samples = buySampleMapper.selectPage(pagination, new EntityWrapper<BuySample>()
                    .eq("enterprise_id", userInfo.getOrgId())
                    .eq("factory_id", userInfo.getFactoryId())
                    .eq("quality_exam_status", EnumSampleRelation.QualityAssayStatus.ALREADY.getKey())
                    .eq("assay_status", EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey())
                    .eq("status", Status.TRUE.getKey())
                    .orderBy("assay_time", false));
        }
        if (CollectionUtils.isEmpty(samples)) {
            dto.setCounts(ZERO);
            dto.setSampleResultDTOs(sampleResultDTOs);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
        }
        for (BuySample buySample : samples) {
            SampleResultDTO sampleResultDTO = new SampleResultDTO();
            sampleResultDTO.setSampleCode(buySample.getSampleCode());
            // 修改 当合同不存在时
            if (!StringUtils.isEmpty(buySample.getContractBusinessId())) {
                BuyContractBasic contractBasic = buyContractBasicMapper
                        .selectOne(new BuyContractBasic().setContractBusinessId(buySample.getContractBusinessId())
                                .setStatus(Status.TRUE.getKey()));
                if (!ObjectUtils.isEmpty(contractBasic)) {
                    sampleResultDTO.setQualityRequirement(contractBasic.getQualityRequirement());
                }
            }
            sampleResultDTO.setAssayPerson(buySample.getAssayPerson());
            sampleResultDTO.setCreateTime(buySample.getCreateTime());
            sampleResultDTO.setProductName(buySample.getProductName());
            sampleResultDTO.setAssayTime(buySample.getAssayTime());
            // 根据样品id查询分析项及结果（输出的）
            List<SampleAssayResult> assayResults = sampleAssayResultMapper.selectList(
                    new EntityWrapper<SampleAssayResult>()
                            .eq("sample_code", buySample.getSampleCode())
                            .eq("type", EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                            .eq("status", Status.TRUE.getKey()));
            List<SampleAssayResultDTO> list = BeanUtils.assemble(SampleAssayResultDTO.class, assayResults);
            // 添加化验单位
            list.forEach(a -> {
                if (a.getTestUnit() != null && EnumSampleRelation.ProductUnit.PERCENTAGE.getKey().equals(a.getTestUnit())) {
                    a.setUnitString(EnumSampleRelation.ProductUnit.PERCENTAGE.getValue());
                } else if (a.getTestUnit() != null && EnumSampleRelation.ProductUnit.EXTREME_RATIO.getKey().equals(a.getTestUnit())) {
                    a.setUnitString(EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue());
                }
            });
            sampleResultDTO.setAssays(list);
            sampleResultDTO.setAssayResult(buySample.getAssayResult());
            sampleResultDTO.setProductSpecId(buySample.getProductSpecId());
            sampleResultDTO.setProductSpecName(buySample.getProductSpecName());
            // 磅房及备注
            List<CarRemarkDTO> carRemarkDTOs = new ArrayList<>();
            List<BuySampleWeightRelation> weightlist = buySampleWeightRelationMapper.selectList(new EntityWrapper<BuySampleWeightRelation>()
                    .eq("sample_code", buySample.getSampleCode())
                    .eq("status", Status.TRUE.getKey()));
            Set<String> machineIds = weightlist.stream().map(a -> a.getMachineId()).collect(Collectors.toSet());
            for (String machineId : machineIds) {
            	CarRemarkDTO carRemarkDTO = new CarRemarkDTO();
            	// 查询磅单
                BuyWeightMachine weightMachine = buyWeightMachineService.selectOne(new EntityWrapper<>(
                		new BuyWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey())));
                if(!ObjectUtils.isEmpty(weightMachine)) {
                    carRemarkDTO.setTrainNumber(weightMachine.getTrainNumber());
                    carRemarkDTO.setRemark(weightMachine.getRemark());
                }
                carRemarkDTOs.add(carRemarkDTO);
            }
            sampleResultDTO.setCarRemarks(carRemarkDTOs);
            sampleResultDTOs.add(sampleResultDTO);
        }
        dto.setCounts(samples.size());
        dto.setSampleResultDTOs(sampleResultDTOs);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
    }

    /**
     * 采购采购商重新化验
     */
    @Override
    public ResponseResult<ResCodeEnum> purchaserReAssay(PurchaserReAssayBuyRQ rq, AuthPlatformUserInfo userInfo) {
        // 磅单id
        String machineId = rq.getMachineId();
        // 查询磅单
        BuyWeightMachine weightMachine = buyWeightMachineService.selectOne(new EntityWrapper<>(new BuyWeightMachine().setMachineId(machineId).setStatus(1)));
        if (ObjectUtils.isEmpty(weightMachine)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.WEIGHT_MACHINE_NOT_FOUND);
        }
        weightMachine.setSampleStatus(EnumSampleRelation.SampleStatus.NOT_YET.getKey())
                .setQualityExamStatus(null)
                .setAssayResult(null)
                .setProductSpecId(null)
                .setProductSpecName(null)
                .setModifyId(userInfo.getId())
                .setModifier(userInfo.getName())
                .setModifyTime(new Date());
        buyWeightMachineService.updateById(weightMachine);
        // 弃用该磅单下的样品
        List<BuySampleWeightRelation> relationList = buySampleWeightRelationService.selectList(new EntityWrapper<>(new BuySampleWeightRelation()
                .setMachineId(machineId)
                .setStatus(Status.TRUE.getKey())));
        if (!CollectionUtils.isEmpty(relationList)) {
            // 弃用该磅单下化验完成的样品
            Set<String> codeList = relationList.stream().map(BuySampleWeightRelation::getSampleCode).collect(Collectors.toSet());
            if (!CollectionUtils.isEmpty(codeList)) {
                BuySample sample = new BuySample().setAssayStatus(EnumSampleRelation.SampleAssayStatus.ABANDON.getKey())
                        .setAbandonId(userInfo.getId())
                        .setAbandonPerson(userInfo.getName())
                        .setAbandonReason("重新化验-reAssay")
                        .setAbandonTime(new Date())
                        .setModifyId(userInfo.getId())
                        .setModifier(userInfo.getName())
                        .setModifyTime(new Date());
                this.update(sample, new EntityWrapper<>(new BuySample()
                        .setStatus(Status.TRUE.getKey())
                        .setAssayStatus(EnumSampleRelation.SampleAssayStatus.ALREADY_ASSAY.getKey()))
                        .in("sample_code", codeList));
            }
            // 删除该榜单下的中间表数据
            relationList.forEach(a -> a.setStatus(Status.FALSE.getKey()));
            buySampleWeightRelationService.updateBatchById(relationList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<SampleResultDTO> getBuySampleInfo(String sampleCode, AuthPlatformUserInfo userInfo) {
        SampleResultDTO sampleResultDTO = new SampleResultDTO();
        BuySample sample = buySampleMapper.selectOne(new BuySample()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setSampleCode(sampleCode)
                .setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(sample)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SMAPLE_NOT_FOUND);
        }
        sampleResultDTO.setSampleCode(sample.getSampleCode());
        // 修改 当合同不存在时
        if (!StringUtils.isEmpty(sample.getContractBusinessId())) {
            BuyContractBasic contractBasic = buyContractBasicMapper
                    .selectOne(new BuyContractBasic().setContractBusinessId(sample.getContractBusinessId())
                            .setStatus(Status.TRUE.getKey()));
            if (!ObjectUtils.isEmpty(contractBasic)) {
                sampleResultDTO.setQualityRequirement(contractBasic.getQualityRequirement());
            }
        }
        sampleResultDTO.setAssayPerson(sample.getAssayPerson());
        sampleResultDTO.setCreateTime(sample.getCreateTime());
        sampleResultDTO.setProductName(sample.getProductName());
        sampleResultDTO.setAssayTime(sample.getAssayTime());
        sampleResultDTO.setProductSpecId(sample.getProductSpecId());
        sampleResultDTO.setProductSpecName(sample.getProductSpecName());
        // 根据样品id查询分析项及结果（输出的）
        List<SampleAssayResult> assayResults = sampleAssayResultMapper.selectList(
                new EntityWrapper<SampleAssayResult>()
                        .eq("sample_code", sample.getSampleCode())
                        .eq("type", EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                        .eq("status", Status.TRUE.getKey()));
        List<SampleAssayResultDTO> list = BeanUtils.assemble(SampleAssayResultDTO.class, assayResults);
        // 添加化验单位
        list.forEach(a -> {
            if (a.getTestUnit() != null && EnumSampleRelation.ProductUnit.PERCENTAGE.getKey().equals(a.getTestUnit())) {
                a.setUnitString(EnumSampleRelation.ProductUnit.PERCENTAGE.getValue());
            } else if (a.getTestUnit() != null && EnumSampleRelation.ProductUnit.EXTREME_RATIO.getKey().equals(a.getTestUnit())) {
                a.setUnitString(EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue());
            }
        });
        // 磅房及备注
        List<CarRemarkDTO> carRemarkDTOs = new ArrayList<>();
        List<BuySampleWeightRelation> weightlist = buySampleWeightRelationMapper.selectList(new EntityWrapper<BuySampleWeightRelation>()
                .eq("sample_code", sample.getSampleCode())
                .eq("status", Status.TRUE.getKey()));
        Set<String> machineIds = weightlist.stream().map(a -> a.getMachineId()).collect(Collectors.toSet());
        for (String machineId : machineIds) {
        	CarRemarkDTO carRemarkDTO = new CarRemarkDTO();
        	// 查询磅单
            BuyWeightMachine weightMachine = buyWeightMachineService.selectOne(new EntityWrapper<>(
            		new BuyWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey())));
            if(!ObjectUtils.isEmpty(weightMachine)) {
                carRemarkDTO.setTrainNumber(weightMachine.getTrainNumber());
                carRemarkDTO.setRemark(weightMachine.getRemark());
            }
            carRemarkDTOs.add(carRemarkDTO);
        }
        sampleResultDTO.setCarRemarks(carRemarkDTOs);
        sampleResultDTO.setAssays(list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, sampleResultDTO);
    }

    @Override
    public ResponseResult<BuyCarSampleMsgDTO> getCarSampleInfo(String machineId, AuthPlatformUserInfo userInfo) {
        BuyCarSampleMsgDTO dto = new BuyCarSampleMsgDTO();
        BuyWeightMachine buyWeightMachine = buyWeightMachineMapper.selectOne(new BuyWeightMachine()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setMachineId(machineId)
                .setStatus(Status.TRUE.getKey()));
        List<BuySample> samples = buySampleMapper.selectList(new EntityWrapper<BuySample>().where(
                "sample_code in (SELECT a.sample_code FROM buy_sample_weight_relation a WHERE a.machine_id = {0} and a.`status` =1)"
                        + "and status = 1 and assay_status = 3", buyWeightMachine.getMachineId()));
        dto.setContractNum(buyWeightMachine.getContractNum());
        dto.setContractBusinessId(buyWeightMachine.getContractBusinessId());
        dto.setProductId(buyWeightMachine.getProductId());
        dto.setProductName(buyWeightMachine.getProductName());
        dto.setProductSpecId(buyWeightMachine.getProductSpecId());
        dto.setProductSpecName(buyWeightMachine.getProductSpecName());
        dto.setRemark(buyWeightMachine.getRemark());
        // 修改 当合同不存在时
        if (!StringUtils.isEmpty(buyWeightMachine.getContractBusinessId())) {
            BuyContractBasic contractBasic = buyContractBasicMapper
                    .selectOne(new BuyContractBasic().setContractBusinessId(buyWeightMachine.getContractBusinessId())
                            .setStatus(Status.TRUE.getKey()));
            if (!ObjectUtils.isEmpty(contractBasic)) {
                dto.setQualityRequirement(contractBasic.getQualityRequirement());
            }
        }
        List<SampleResultDTO> resultDTOs = new ArrayList<SampleResultDTO>();
        for (BuySample buySample : samples) {
            SampleResultDTO sampleResultDTO = new SampleResultDTO();
            sampleResultDTO.setSampleCode(buySample.getSampleCode());
            sampleResultDTO.setCreateTime(buySample.getCreateTime());
            sampleResultDTO.setAssayTime(buySample.getAssayTime());
            // 根据样品id查询分析项及结果（输出的）
            List<SampleAssayResult> assayResults = sampleAssayResultMapper.selectList(
                    new EntityWrapper<SampleAssayResult>()
                            .eq("sample_code", buySample.getSampleCode())
                            .eq("type", EnumSampleRelation.SampleAssayResultType.OUT.getKey())
                            .eq("status", Status.TRUE.getKey()));
            sampleResultDTO.setAssayResult(buySample.getAssayResult());
            List<SampleAssayResultDTO> list = BeanUtils.assemble(SampleAssayResultDTO.class, assayResults);
            // 添加化验单位
            list.forEach(a -> {
                if (a.getTestUnit() != null && EnumSampleRelation.ProductUnit.PERCENTAGE.getKey().equals(a.getTestUnit())) {
                    a.setUnitString(EnumSampleRelation.ProductUnit.PERCENTAGE.getValue());
                } else if (a.getTestUnit() != null && EnumSampleRelation.ProductUnit.EXTREME_RATIO.getKey().equals(a.getTestUnit())) {
                    a.setUnitString(EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue());
                }
            });
            sampleResultDTO.setAssays(list);
            sampleResultDTO.setProductSpecId(buySample.getProductSpecId());
            sampleResultDTO.setProductSpecName(buySample.getProductSpecName());
            resultDTOs.add(sampleResultDTO);
        }
        dto.setSampleResultDTOs(resultDTOs);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @Override
    public List<BuyCarSampleDTO> getCarSimple(String contractBusinessId, String batchId) {
        List<BuyCarSampleDTO> buyCarSampleDTOs = new ArrayList<BuyCarSampleDTO>();
        List<BuyWeightMachine> machines = buyWeightMachineMapper.selectList(new EntityWrapper<BuyWeightMachine>()
                .eq("contract_business_id", contractBusinessId)
                .eq("batch_id", batchId)
                .eq("quality_exam_status", EnumSampleRelation.QualityExamStatus.YES.getKey())
                .eq("status", Status.TRUE.getKey()));
        buyCarSampleDTOs = BeanUtils.assemble(BuyCarSampleDTO.class, machines);
        for (BuyCarSampleDTO buyCarSampleDTO : buyCarSampleDTOs) {
            List<BuySample> samples = buySampleMapper.selectList(new EntityWrapper<BuySample>().where(
                    "sample_code in (SELECT a.sample_code FROM buy_sample_weight_relation a WHERE a.machine_id = {0} and a.`status` =1)"
                            + "and status = 1 and assay_status = 3", buyCarSampleDTO.getMachineId()));
            // 再次化验按钮是否显示
            if (CollectionUtils.isEmpty(samples)) {
                buyCarSampleDTO.setRebackBtn(0);
            } else {
                buyCarSampleDTO.setRebackBtn(1);
            }
        }
        return buyCarSampleDTOs;
    }

    /**
     * 保存样品化验结果
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveAssayResult(SampleAssayResultSaveRQ rq, AuthPlatformUserInfo userInfo) {
        String sampleCode = rq.getSampleCode();
        Integer productId = rq.getProductId();
        List<SampleAssayResultDTO> resultList = rq.getResultList();
        // 查询对应样品
        BuySample sample = this.selectOne(new EntityWrapper<>(new BuySample().setSampleCode(sampleCode)
                .setEnterpriseId(userInfo.getOrgId())
                .setStatus(1)));
        if (sample == null) {
            log.info("未查询到该样品，方法：{}", "saveAssayResult");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAMPLE_NOT_FOUND);
        }
        // 多次保存 将以前保存的化验结果逻辑删除
        sampleAssayResultService.update(new SampleAssayResult().setStatus(0),
                new EntityWrapper<>(new SampleAssayResult()
                        .setSampleCode(rq.getSampleCode())
                        .setStatus(1)));
        // 查询样品输出项,并计算保存输出项结果
        sampleService.computeAndsaveAssayResult(sampleCode, productId, resultList,
                EnumSampleRelation.SampleAssayResultBusinessType.PURCHASE.getKey(), userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @Override
    public ResponseResult<List<BuyUnqualifiedCarDTO>> getUnqualifiedCarList(String contractBusinessId,
                                                                            AuthPlatformUserInfo userInfo) {
        List<BuyWeightMachine> machines = buyWeightMachineMapper.selectList(new EntityWrapper<BuyWeightMachine>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("contract_business_id", contractBusinessId)
                .eq("assay_result", EnumSampleRelation.QualityAssayResult.NO.getKey())
                .eq("in_storage_confirm", EnumSampleRelation.InStorageConfirmType.NO.getKey())
                .eq("status", Status.TRUE.getKey())
                .orderBy("weighing_time", false));
        List<BuyUnqualifiedCarDTO> list = BeanUtils.assemble(BuyUnqualifiedCarDTO.class, machines);
        for (BuyUnqualifiedCarDTO buyUnqualifiedCarDTO : list) {
            BuyLogisticsBatch buyLogisticsBatch = buyLogisticsBatchMapper.selectOne(new BuyLogisticsBatch()
                    .setBatchId(buyUnqualifiedCarDTO.getBatchId())
                    .setStatus(Status.TRUE.getKey()));
            if (!ObjectUtils.isEmpty(buyLogisticsBatch)) {
                buyUnqualifiedCarDTO.setBatchName(buyLogisticsBatch.getBatchName());
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }

    @Override
    public ResponseResult<SampleAssayDetailDTO> getSampleAssayDetailByCode(Object o, String sampleCode) {
        BuySample sample = (BuySample) o;
        if (!Objects.equals(sample.getAssayStatus(), EnumSampleRelation.SampleAssayStatus.ASSAYING.getKey())) {
            log.info("改条码的样品不在采购化验中样品的列表，样品code：{}", sampleCode);
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_IN_PREPARE_ASSAY);
        }
        SampleAssayDetailDTO dto = BeanUtils.copyProperties(sample, SampleAssayDetailDTO.class);
        dto.setBusinessType(EnumSampleRelation.SampleAssayResultBusinessType.PURCHASE.getKey());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @Override
    public ResponseResult<List<BuyCarDTO>> getCarList(String contractBusinessId, AuthPlatformUserInfo userInfo,
                                                      Pagination pagination) {
        List<BuyWeightMachine> machines = buyWeightMachineMapper.selectPage(pagination, new EntityWrapper<BuyWeightMachine>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("contract_business_id", contractBusinessId)
                .eq("quality_exam_status", EnumSampleRelation.QualityExamStatus.YES.getKey())
                .eq("status", Status.TRUE.getKey())
                .orderBy("weighing_time", false));
        List<BuyCarDTO> list = BeanUtils.assemble(BuyCarDTO.class, machines);
        for (BuyCarDTO buyCarDTO : list) {
            BuyLogisticsBatch buyLogisticsBatch = buyLogisticsBatchMapper.selectOne(new BuyLogisticsBatch()
                    .setBatchId(buyCarDTO.getBatchId())
                    .setStatus(Status.TRUE.getKey()));
            if (!ObjectUtils.isEmpty(buyLogisticsBatch)) {
                buyCarDTO.setBatchName(buyLogisticsBatch.getBatchName());
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));
    }

}
