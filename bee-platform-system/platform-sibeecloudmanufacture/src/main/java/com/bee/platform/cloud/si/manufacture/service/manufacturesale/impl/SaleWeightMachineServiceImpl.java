package com.bee.platform.cloud.si.manufacture.service.manufacturesale.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.FinishedProductBeOutOfStorageMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleContractBasicMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleWeightMachineFileMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleWeightMachineMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.enums.EnumWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleWeightMachineService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumGenerateIdModule;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.GenerateIdService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;


/**
 * @Description 地磅数据信息 服务实现类
 * @author chenxm66777123
 * @Date 2019/9/26 16:01
 * @version 1.0.0
 */
@Service
@Slf4j
public class SaleWeightMachineServiceImpl extends ServiceImpl<SaleWeightMachineMapper, SaleWeightMachine> implements SaleWeightMachineService {

    @Autowired
    private SaleWeightMachineMapper saleWeightMachineMapper;

    @Autowired
    private SaleWeightMachineFileMapper saleWeightMachineFileMapper;

    @Autowired
    private GenerateIdService generateIdService;

    @Autowired
    private SaleContractBasicMapper saleContractBasicMapper;

    @Autowired
    private StorageService storageService;

    @Autowired
    private FinishedProductBeOutOfStorageMapper finishedProductBeOutOfStorageMapper;

    /**
     * @Description 保存地磅数据信息
     * @author chenxm66777123
     * @Date 2019/9/24 9:23
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> saveSaleWeightMachine(WeightMachineRq weightMachineRq, AuthPlatformUserInfo userInfo,Integer dataSource) {
        //判断是否传过来榜单id
        SaleWeightMachine saleWeightMachine;

        //提前处理附件信息
        List<SaleWeightMachineFile> saleWeightMachineFileList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(weightMachineRq.getFiles())) {
            saleWeightMachineFileList = BeanUtils.assemble(SaleWeightMachineFile.class, weightMachineRq.getFiles());
            saleWeightMachineFileList.stream().forEach(obj -> {
                obj.setMachineId(weightMachineRq.getMachineId());
                obj.setCreateId(userInfo.getId());
                obj.setStatus(Status.TRUE.getKey());
                obj.setCreator(userInfo.getName());
                obj.setCreateTime(new Date());
            });
        }

        //保存操作，如果没有磅单编号，则进行保存操作
        if (StringUtils.isBlank(weightMachineRq.getMachineId())) {
            //复制传入参数
            saleWeightMachine = BeanUtils.copyProperties(weightMachineRq, SaleWeightMachine.class);

            //生成id
            String generateBusinessId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.SALE_WEIGHT_MACHINE.getKey());

            saleWeightMachine.setMachineId(generateBusinessId);
            saleWeightMachine.setEnterpriseId(userInfo.getOrgId());
            saleWeightMachine.setFactoryId(userInfo.getFactoryId());

            BigDecimal inFactoryWeight = BigDecimal.ZERO;
            if (!ObjectUtils.isEmpty(weightMachineRq.getInFactoryWeight())) {
                //称重时间
                saleWeightMachine.setWeighingTime(new Date());
                //司磅员
                saleWeightMachine.setWeightMan(userInfo.getName());

                //千克转换为吨
                inFactoryWeight = weightMachineRq.getInFactoryWeight();
                saleWeightMachine.setInFactoryWeight(inFactoryWeight);
                Integer weightType = ObjectUtils.isEmpty(weightMachineRq.getInFactoryWeightByManual()) ? 0 : weightMachineRq.getInFactoryWeightByManual();
                //是否手动修改  0 自动 1手动
                saleWeightMachine.setInFactoryWeightByManual(weightType);
                //是否确认称重 0 未确认 1 确认
                saleWeightMachine.setInFactoryWeightIsConfirm(Status.TRUE.getKey());
            }
            BigDecimal outFactoryWeight = BigDecimal.ZERO;

            if (!ObjectUtils.isEmpty(weightMachineRq.getOutFactoryWeight())) {
                //千克转换为吨
                outFactoryWeight = weightMachineRq.getOutFactoryWeight();
                saleWeightMachine.setOutFactoryWeight(outFactoryWeight);
                Integer weightType = ObjectUtils.isEmpty(weightMachineRq.getOutFactoryWeightByManual()) ? 0 : weightMachineRq.getOutFactoryWeightByManual();
                //是否确认称重 0 未确认 1 确认
                saleWeightMachine.setOutFactoryWeightIsConfirm(Status.TRUE.getKey());
                //是否手动修改  0 自动 1手动
                saleWeightMachine.setOutFactoryWeightByManual(weightType);
            }

            saleWeightMachine.setIsPushStorage(Status.FALSE.getKey());

            if(EnumWeightMachine.DataSource.ADD.getValue().equals(dataSource)
                    && inFactoryWeight.compareTo(BigDecimal.ZERO) <= 0 ){
                return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
            }

            //新增
            if(EnumWeightMachine.DataSource.ADD.getValue().equals(dataSource)){
                FinishedProductBeOutOfStorageDTO finishedProductBeOutOfStorageDTO = new FinishedProductBeOutOfStorageDTO();
                finishedProductBeOutOfStorageDTO.setContractId(ObjectUtils.isEmpty(saleWeightMachine.getContractNum())?"":saleWeightMachine.getContractNum());
                finishedProductBeOutOfStorageDTO.setCarrierId(
                        ObjectUtils.isEmpty(saleWeightMachine.getCarrierId())? 0 : saleWeightMachine.getCarrierId().longValue());
                finishedProductBeOutOfStorageDTO.setCarrierName(saleWeightMachine.getCarrierName());
                finishedProductBeOutOfStorageDTO.setLicensePlateNumber(saleWeightMachine.getTrainNumber());
                finishedProductBeOutOfStorageDTO.setProductName(saleWeightMachine.getProductName());
                finishedProductBeOutOfStorageDTO.setProductId(saleWeightMachine.getProductId());
                finishedProductBeOutOfStorageDTO.setProductNumber(ObjectUtils.isEmpty(saleWeightMachine.getCargoWeight())? BigDecimal.ZERO :saleWeightMachine.getCargoWeight());
                finishedProductBeOutOfStorageDTO.setDriverName(saleWeightMachine.getDriver());
                String contact = ObjectUtils.isEmpty(saleWeightMachine.getContact())? "":saleWeightMachine.getContact();
                finishedProductBeOutOfStorageDTO.setContact(contact);
                //2019-12-04 add by chen
                finishedProductBeOutOfStorageDTO.setMachineId(generateBusinessId);
                storageService.saveFinishedProductOutOfStorage(finishedProductBeOutOfStorageDTO, userInfo);
                //新增直接推送
                saleWeightMachine.setIsPushStorage(Status.TRUE.getKey());
            }

            //新增数据来源
            saleWeightMachine.setDataSource(dataSource);

            saleWeightMachine.setNetWeight(BigDecimal.ZERO);

            //实际到厂时间取确认榜单的时间
            saleWeightMachine.setArrivalTime(new Date());

            saleWeightMachine.setDeliveryCompany(userInfo.getOrg_name());

            //地磅处理状态信息
            saleWeightMachine.setStatus(Status.TRUE.getKey());
            saleWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.NO.getValue());
            saleWeightMachine.setSampleStatus(EnumSampleRelation.SampleResults.NO.getKey());
            saleWeightMachine.setAssayResult(Status.FALSE.getKey());

            //创建人信息
            saleWeightMachine.setCreateId(userInfo.getId());
            saleWeightMachine.setCreator(userInfo.getName());
            saleWeightMachine.setCreateTime(new Date());

            saleWeightMachine.setModifyId(userInfo.getId());
            saleWeightMachine.setModifier(userInfo.getName());
            saleWeightMachine.setModifyTime(new Date());


            saleWeightMachineMapper.insert(saleWeightMachine);

            if (!CollectionUtils.isEmpty(saleWeightMachineFileList)) {
                saleWeightMachineFileMapper.batchSaveFile(saleWeightMachineFileList);
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_SUCCESS);
        }

        //进行更新操作

        saleWeightMachine = saleWeightMachineMapper.selectOne(new SaleWeightMachine().setMachineId(weightMachineRq.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL_AND_NOT_FOUND_DATA);
        }

        SaleWeightMachine updateSaleWeightMachine = BeanUtils.copyProperties(weightMachineRq, SaleWeightMachine.class);

        //地磅处理信息处理
        updateSaleWeightMachine.setStatus(saleWeightMachine.getStatus());
        //是否称重，确认后置位已称重
        updateSaleWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.YES.getValue());
        //通知取样状态
        updateSaleWeightMachine.setSamplePushStatus(Status.TRUE.getKey());
        //通知取样时间
        updateSaleWeightMachine.setSamplePushTime(new Date());
        //是否需要取样
        updateSaleWeightMachine.setSampleStatus(saleWeightMachine.getSampleStatus());
        //是否化验
        updateSaleWeightMachine.setAssayResult(saleWeightMachine.getAssayResult());
        //状态
        updateSaleWeightMachine.setStatus(saleWeightMachine.getStatus());

        //司磅员
        updateSaleWeightMachine.setWeightMan(userInfo.getName());

        //创建者信息
        updateSaleWeightMachine.setCreateId(saleWeightMachine.getCreateId());
        updateSaleWeightMachine.setCreator(saleWeightMachine.getCreator());
        updateSaleWeightMachine.setCreateTime(saleWeightMachine.getCreateTime());
        updateSaleWeightMachine.setModifyId(userInfo.getId());
        updateSaleWeightMachine.setModifier(userInfo.getName());
        updateSaleWeightMachine.setModifyTime(new Date());
        saleWeightMachineMapper.updateAllColumnById(updateSaleWeightMachine);

        //保存磅单附件信息
        saleWeightMachineFileMapper.batchSaveFile(saleWeightMachineFileList);


        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
    }


    /**
     * @Description 根据企业查询称重信息
     * @author chenxm66777123
     * @Date 2019/9/24 11:33
     * @version 1.0.0
     */
    @Override
    public List<WeightMachineDTO> getSaleWeightMachineList(Integer orgId, Pagination pagination) {
        List<SaleWeightMachine> saleWeightMachines = saleWeightMachineMapper.
                selectPage(pagination,new EntityWrapper<SaleWeightMachine>()
                        .where("status = 1 and enterprise_id = {0} and is_weight = {1}", orgId, Status.FALSE.getKey()).orderBy("create_time",false));
        List<WeightMachineDTO> list = BeanUtils.assemble(WeightMachineDTO.class, saleWeightMachines);
        if (!CollectionUtils.isEmpty(list)) {
            list.stream().forEach(obj -> {
                List<SaleWeightMachineFile> saleWeightMachineFiles = saleWeightMachineFileMapper.selectList(
                        new EntityWrapper<SaleWeightMachineFile>().where("status = 1 and machine_id = {0}", obj.getMachineId()));
                obj.setFiles(BeanUtils.assemble(CommonFileRq.class, saleWeightMachineFiles));

                //处理单位换算问题 吨转化为kg
                //出厂重量
                obj.setOutFactoryWeight(tonTokg(obj.getOutFactoryWeight()));
                //进厂重量
                obj.setInFactoryWeight(tonTokg(obj.getInFactoryWeight()));
                //净厂重量
                obj.setNetWeight(tonTokg(obj.getNetWeight()));
            });
        }
        return list;
    }

    /**
     * @Description 根据企业获取已称重详情（总览）
     * @author chenxm66777123
     * @Date 2019/9/24 11:33
     * @version 1.0.0
     */
    @Override
    public List<WeightMachineTotalDTO> getWeightMachineListTotal(Integer orgId, Pagination pagination) {
        List<WeightMachineTotalDTO> list = saleWeightMachineMapper.getWeightMachineListTotal(orgId, pagination);
        //根据合同编号去获取信息
        list.stream().forEach(obj->{
            SaleContractBasic saleContractBasic =  saleContractBasicMapper.selectOne(
                    new SaleContractBasic().setContractNum(obj.getContractNum()).setStatus(Status.TRUE.getKey())
            );
            if(!ObjectUtils.isEmpty(saleContractBasic)){
                obj.setContractWeight(saleContractBasic.getQuantity());
            }else{
                obj.setContractWeight(BigDecimal.ZERO);
            }
        });
        return list;
    }


    /**
     * @Description 根据企业获取已称重详情（详细信息）
     * @author chenxm66777123
     * @Date 2019/9/24 11:33
     * @version 1.0.0
     */
    @Override
    public List<WeightMachineDTO> getWeightMachineListDetails(Integer orgId, String contractNum, Pagination pagination) {
        List<SaleWeightMachine> saleWeightMachine = saleWeightMachineMapper.
                selectPage(pagination, new EntityWrapper<SaleWeightMachine>()
                        .where("status = 1 and enterprise_id = {0} and is_weight = {1} and contract_num = {2}"
                                , orgId, Status.TRUE.getKey(),contractNum).orderBy("create_time",false));
        List<WeightMachineDTO> list = BeanUtils.assemble(WeightMachineDTO.class, saleWeightMachine);
        if (!CollectionUtils.isEmpty(list)) {
            list.stream().forEach(obj -> {
                List<SaleWeightMachineFile> saleWeightMachineFiles = saleWeightMachineFileMapper.selectList(
                        new EntityWrapper<SaleWeightMachineFile>().where("status = 1 and machine_id = {0}", obj.getMachineId()));
                obj.setFiles(BeanUtils.assemble(CommonFileRq.class, saleWeightMachineFiles));

                //处理单位换算问题 吨转化为kg
                //净厂重量
                obj.setNetWeight(tonTokg(obj.getNetWeight()));
                //出厂重量
                obj.setOutFactoryWeight(tonTokg(obj.getOutFactoryWeight()));
                //进厂重量
                obj.setInFactoryWeight(tonTokg(obj.getInFactoryWeight()));


            });
        }
        return list;
    }


    /**
     * @Description 保存称重信息
     * @author chenxm66777123
     * @Date 2019/9/28 14:15
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> saleConfirmWeight(ConfirmWeightMachineRq confirmWeightMachineRq,AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(confirmWeightMachineRq.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }
        Integer type = confirmWeightMachineRq.getType();
        //进厂称重
        if (EnumWeightMachine.ConfirmWeightType.IN_FACTORY_WEIGHT.getValue().equals(type)) {

            //千克转换为吨
            BigDecimal inFactoryWeight = confirmWeightMachineRq.getInFactoryWeight();

            saleWeightMachine.setInFactoryWeight(inFactoryWeight);
            //是否确认称重 0 未确认 1 确认
            saleWeightMachine.setInFactoryWeightIsConfirm(Status.TRUE.getKey());
            //是否手动修改  0 自动 1手动
            saleWeightMachine.setInFactoryWeightByManual(confirmWeightMachineRq.getInFactoryWeightByManual());
            //称重时间
            saleWeightMachine.setWeighingTime(confirmWeightMachineRq.getWeighingTime());

            BigDecimal outData = saleWeightMachine.getOutFactoryWeight() == null ? BigDecimal.ZERO :saleWeightMachine.getOutFactoryWeight();
            //修改净重
            BigDecimal netWeight = outData.subtract(inFactoryWeight).subtract(bigDecimalIsNull(saleWeightMachine.getDeductWeight()));
            saleWeightMachine.setNetWeight(netWeight);

            //司磅员
            saleWeightMachine.setWeightMan(userInfo.getName());

            //修改者信息
            saleWeightMachine.setModifyId(userInfo.getId());
            saleWeightMachine.setModifier(userInfo.getName());
            saleWeightMachine.setModifyTime(new Date());

            if(Status.FALSE.getKey().equals(saleWeightMachine.getIsPushStorage())){
                FinishedProductBeOutOfStorageDTO finishedProductBeOutOfStorageDTO = new FinishedProductBeOutOfStorageDTO();
                finishedProductBeOutOfStorageDTO.setContractId(ObjectUtils.isEmpty(saleWeightMachine.getContractNum())?"":saleWeightMachine.getContractNum());
                finishedProductBeOutOfStorageDTO.setCarrierId(
                        ObjectUtils.isEmpty(saleWeightMachine.getCarrierId())? 0 : saleWeightMachine.getCarrierId().longValue());
                finishedProductBeOutOfStorageDTO.setCarrierName(saleWeightMachine.getCarrierName());
                finishedProductBeOutOfStorageDTO.setLicensePlateNumber(saleWeightMachine.getTrainNumber());
                finishedProductBeOutOfStorageDTO.setProductId(saleWeightMachine.getProductId());
                finishedProductBeOutOfStorageDTO.setProductName(saleWeightMachine.getProductName());
                finishedProductBeOutOfStorageDTO.setProductNumber(ObjectUtils.isEmpty(saleWeightMachine.getCargoWeight())? BigDecimal.ZERO :saleWeightMachine.getCargoWeight());
                finishedProductBeOutOfStorageDTO.setDriverName(saleWeightMachine.getDriver());
                String contact = ObjectUtils.isEmpty(saleWeightMachine.getContact())? "":saleWeightMachine.getContact();
                finishedProductBeOutOfStorageDTO.setContact(contact);
                //2019-12-04 add by chen
                finishedProductBeOutOfStorageDTO.setMachineId(saleWeightMachine.getMachineId());
                storageService.saveFinishedProductOutOfStorage(finishedProductBeOutOfStorageDTO, userInfo);
                saleWeightMachine.setIsPushStorage(Status.TRUE.getKey());
            }

            saleWeightMachineMapper.updateById(saleWeightMachine);
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
        }
        //出厂称重
        if (EnumWeightMachine.ConfirmWeightType.OUT_FACTORY_WEIGHT.getValue().equals(type)) {
            //千克转换为吨
            BigDecimal outFactoryWeight = confirmWeightMachineRq.getOutFactoryWeight();

            saleWeightMachine.setOutFactoryWeight(outFactoryWeight);
            //是否确认称重 0 未确认 1 确认
            saleWeightMachine.setOutFactoryWeightIsConfirm(Status.TRUE.getKey());
            //是否手动修改  0 自动 1手动
            saleWeightMachine.setOutFactoryWeightByManual(confirmWeightMachineRq.getOutFactoryWeightByManual());
            //修改净重
            BigDecimal inData = saleWeightMachine.getInFactoryWeight() == null ? BigDecimal.ZERO :saleWeightMachine.getInFactoryWeight();

            BigDecimal netWeight = saleWeightMachine.getOutFactoryWeight().subtract(inData).subtract(bigDecimalIsNull(saleWeightMachine.getDeductWeight()));
            saleWeightMachine.setNetWeight(netWeight);

            //司磅员
            saleWeightMachine.setWeightMan(userInfo.getName());

            //修改者信息
            saleWeightMachine.setModifyId(userInfo.getId());
            saleWeightMachine.setModifier(userInfo.getName());
            saleWeightMachine.setModifyTime(new Date());

            saleWeightMachine.setWeighingTime(new Date());

            saleWeightMachineMapper.updateById(saleWeightMachine);
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
    }


    /**
     * @Description 通知取样
     * @author chenxm66777123
     * @Date 2019/9/30 10:15
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> samplePush(String machineId, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }
        //判断进厂重量
        if(ObjectUtils.isEmpty(saleWeightMachine.getInFactoryWeight())
                || saleWeightMachine.getInFactoryWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
        }
        //判断出厂重量
        if(ObjectUtils.isEmpty(saleWeightMachine.getOutFactoryWeight())
                || saleWeightMachine.getOutFactoryWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.OUT_WEIGHT_ZERO);
        }
        // 修改推送信息
        saleWeightMachine.setSamplePushStatus(Status.TRUE.getKey());
        saleWeightMachine.setSamplePushTime(new Date());
        // 修改修改人信息
        saleWeightMachine.setModifyId(userInfo.getId());
        saleWeightMachine.setModifier(userInfo.getName());
        saleWeightMachine.setModifyTime(new Date());
        saleWeightMachineMapper.updateById(saleWeightMachine);
        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> confirmMachine(ConfirmMachineRQ confirmMachineRQ, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(confirmMachineRQ.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }
        //处理附件信息
        List<SaleWeightMachineFile> saleWeightMachineFileList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(confirmMachineRQ.getFiles())) {
            saleWeightMachineFileList = BeanUtils.assemble(SaleWeightMachineFile.class, confirmMachineRQ.getFiles());
            saleWeightMachineFileList.stream().forEach(obj -> {
                obj.setMachineId(confirmMachineRQ.getMachineId());
                obj.setStatus(Status.TRUE.getKey());
                obj.setCreateTime(new Date());
                obj.setCreateId(userInfo.getId());
                obj.setCreator(userInfo.getName());
            });
            //保存磅单附件信息
            saleWeightMachineFileMapper.batchSaveFile(saleWeightMachineFileList);
        }
        //判断进厂重量
        if(ObjectUtils.isEmpty(saleWeightMachine.getInFactoryWeight())
                || saleWeightMachine.getInFactoryWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
        }
        //判断出厂重量
        if(ObjectUtils.isEmpty(saleWeightMachine.getOutFactoryWeight())
                || saleWeightMachine.getOutFactoryWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.OUT_WEIGHT_ZERO);
        }

        if(!ObjectUtils.isEmpty(confirmMachineRQ.getRemark())){
            saleWeightMachine.setRemark(confirmMachineRQ.getRemark());
        }

        if (!ObjectUtils.isEmpty(confirmMachineRQ.getDeductWeight())) {
            //修改净重
            BigDecimal deduct = bigDecimalIsNull(confirmMachineRQ.getDeductWeight());
            //2019-11-04 净重字段的公式修改为：出厂重量-进厂重量-扣重
            BigDecimal netWeight = saleWeightMachine.getOutFactoryWeight().subtract(saleWeightMachine.getInFactoryWeight()).subtract(deduct);

            saleWeightMachine.setNetWeight(netWeight);
            saleWeightMachine.setDeductWeight(deduct);
        }

        //2019-12-13 如果净重等于0 或者为负 则不允许确认磅单
        if(saleWeightMachine.getNetWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.NET_WEIGHT_ZERO);
        }


        //是否称重，确认后置位已称重
        saleWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.YES.getValue());
        //通知取样状态
        saleWeightMachine.setSamplePushStatus(Status.TRUE.getKey());
        //通知取样时间
        saleWeightMachine.setSamplePushTime(new Date());

        //修改者信息
        saleWeightMachine.setModifyId(userInfo.getId());
        saleWeightMachine.setModifier(userInfo.getName());
        saleWeightMachine.setModifyTime(new Date());
        saleWeightMachineMapper.updateAllColumnById(saleWeightMachine);

        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> continueMachine(ConfirmMachineRQ confirmMachineRQ, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(confirmMachineRQ.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }
        //生成id
        String generateBusinessId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.SALE_WEIGHT_MACHINE.getKey());

        SaleWeightMachine newSaleWeightMachine = BeanUtils.copyProperties(saleWeightMachine, SaleWeightMachine.class);
        newSaleWeightMachine.setId(null);
        newSaleWeightMachine.setMachineId(generateBusinessId);
        //处理附件信息
        List<SaleWeightMachineFile> saleWeightMachineFileList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(confirmMachineRQ.getFiles())) {
            saleWeightMachineFileList = BeanUtils.assemble(SaleWeightMachineFile.class, confirmMachineRQ.getFiles());
            saleWeightMachineFileList.stream().forEach(obj -> {
                obj.setStatus(Status.TRUE.getKey());
                obj.setMachineId(saleWeightMachine.getMachineId());
                obj.setCreateTime(new Date());
                obj.setCreateId(userInfo.getId());
                obj.setCreator(userInfo.getName());
            });
            //保存磅单附件信息
            saleWeightMachineFileMapper.batchSaveFile(saleWeightMachineFileList);
        }
        //判断进厂重量
        if(ObjectUtils.isEmpty(saleWeightMachine.getInFactoryWeight())
                || saleWeightMachine.getInFactoryWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
        }
        //判断出厂重量
        if(ObjectUtils.isEmpty(saleWeightMachine.getOutFactoryWeight())
                || saleWeightMachine.getOutFactoryWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.OUT_WEIGHT_ZERO);
        }

        if(!ObjectUtils.isEmpty(confirmMachineRQ.getRemark())){
            saleWeightMachine.setRemark(confirmMachineRQ.getRemark());
        }
        if (!ObjectUtils.isEmpty(confirmMachineRQ.getDeductWeight())) {
            //修改净重
            BigDecimal deduct = bigDecimalIsNull(confirmMachineRQ.getDeductWeight());
            //2019-11-04 净重字段的公式修改为：出厂重量-进厂重量-扣重
            BigDecimal netWeight = saleWeightMachine.getOutFactoryWeight().subtract(saleWeightMachine.getInFactoryWeight()).subtract(deduct);

            saleWeightMachine.setDeductWeight(deduct);
            saleWeightMachine.setNetWeight(netWeight);
        }

        //2019-12-13 如果净重等于0 或者为负 则不允许确认磅单
        if(saleWeightMachine.getNetWeight().compareTo(BigDecimal.ZERO) <= 0){
            return ResponseResult.buildResponseResult(ResCodeEnum.NET_WEIGHT_ZERO);
        }

        //是否称重，确认后置位已称重
        saleWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.YES.getValue());
        //通知取样状态
        saleWeightMachine.setSamplePushStatus(Status.TRUE.getKey());
        //通知取样时间
        saleWeightMachine.setSamplePushTime(new Date());


        //修改者信息
        saleWeightMachine.setModifyId(userInfo.getId());
        saleWeightMachine.setModifier(userInfo.getName());
        saleWeightMachine.setModifyTime(new Date());
        saleWeightMachineMapper.updateById(saleWeightMachine);



        //是否称重，确认后置位已称重
        newSaleWeightMachine.setIsWeight(EnumWeightMachine.IsWeight.NO.getValue());
        newSaleWeightMachine.setRemark("");
        //通知取样状态
        newSaleWeightMachine.setSamplePushStatus(Status.FALSE.getKey());
        //通知取样时间
        newSaleWeightMachine.setSamplePushTime(new Date());
        newSaleWeightMachine.setInFactoryWeight(null);
        newSaleWeightMachine.setOutFactoryWeight(null);
        newSaleWeightMachine.setInFactoryWeightIsConfirm(Status.FALSE.getKey());
        newSaleWeightMachine.setOutFactoryWeightIsConfirm(Status.FALSE.getKey());
        newSaleWeightMachine.setDeductWeight(null);
        //修改者信息
        newSaleWeightMachine.setModifyId(userInfo.getId());
        newSaleWeightMachine.setModifier(userInfo.getName());
        newSaleWeightMachine.setModifyTime(new Date());
        saleWeightMachineMapper.insert(newSaleWeightMachine);

        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
    }

    /**
     * @Description 删除磅单
     * @author chenxm66777123
     * @Date 2019/10/21 17:19
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> delMachine(ConfirmMachineRQ confirmMachineRQ, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(confirmMachineRQ.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }
        saleWeightMachine.setStatus(Status.FALSE.getKey());
        //修改者信息
        saleWeightMachine.setModifyId(userInfo.getId());
        saleWeightMachine.setModifier(userInfo.getName());
        saleWeightMachine.setModifyTime(new Date());

        saleWeightMachineMapper.updateById(saleWeightMachine);
        return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> confirmDeductWeight(String machineId, BigDecimal deductWeight,
              Integer deductWeightByManual, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }
        //判断进厂重量
        if (ObjectUtils.isEmpty(saleWeightMachine.getOutFactoryWeight())
                || saleWeightMachine.getOutFactoryWeight().compareTo(BigDecimal.ZERO) <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.IN_WEIGHT_ZERO);
        }
        //出厂重量
        BigDecimal inFactoryWeight = bigDecimalIsNull(saleWeightMachine.getInFactoryWeight());
        //修改净重
        BigDecimal deduct = bigDecimalIsNull(deductWeight);
        //2019-11-04 净重字段的公式修改为：出厂重量-进厂重量-扣重
        BigDecimal netWeight = saleWeightMachine.getOutFactoryWeight().subtract(inFactoryWeight).subtract(deduct);


        saleWeightMachine.setDeductWeight(deduct);
        saleWeightMachine.setDeductWeightByManual(deductWeightByManual);
        saleWeightMachine.setNetWeight(netWeight);

        //修改者信息
        saleWeightMachine.setModifyId(userInfo.getId());
        saleWeightMachine.setModifier(userInfo.getName());
        saleWeightMachine.setModifyTime(new Date());

        saleWeightMachineMapper.updateById(saleWeightMachine);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 根据车牌号，自动带出最近一次该车所对应的承运商。
     * @author chenxm66777123
     * @Date 2019/11/11 9:51
     * @version 1.0.0
     */
    @Override
    public ResponseResult<CarrierInfoDTO> getLastCarrierByTrainNumber(String trainNumber, AuthPlatformUserInfo userInfo) {
        SaleWeightMachine saleWeightMachine = this.selectOne(
                new EntityWrapper<SaleWeightMachine>()
                        .where("1 =1 and status = 1 and is_weight = 1 and enterprise_id = {0} and factory_id ={1} and train_number ={2}"
                                ,userInfo.getOrgId(),userInfo.getFactoryId(),trainNumber)
                        .orderBy("create_time",false).last("limit 1"));
        if(ObjectUtils.isEmpty(saleWeightMachine)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        CarrierInfoDTO carrierInfoDTO = new  CarrierInfoDTO();
        if(ObjectUtils.isEmpty(saleWeightMachine.getCarrierId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        carrierInfoDTO.setCarrierId(saleWeightMachine.getCarrierId().longValue());
        carrierInfoDTO.setCarrierName(saleWeightMachine.getCarrierName());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,carrierInfoDTO);
    }

    /**
     * @Description 保存备注
     * @author chenxm66777123
     * @Date 2019/11/15 9:56
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> saveRemark(String machineId, String remark, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }
        saleWeightMachine.setRemark(remark);

        //修改者信息
        saleWeightMachine.setModifyId(userInfo.getId());
        saleWeightMachine.setModifier(userInfo.getName());
        saleWeightMachine.setModifyTime(new Date());

        saleWeightMachineMapper.updateById(saleWeightMachine);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @Override
    public ResponseResult<String> updateTrainNumber(String machineId, String trainNumber, AuthPlatformUserInfo userInfo) {
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }

        saleWeightMachine.setTrainNumber(trainNumber);

        //修改者信息
        saleWeightMachine.setModifyId(userInfo.getId());
        saleWeightMachine.setModifier(userInfo.getName());
        saleWeightMachine.setModifyTime(new Date());

        saleWeightMachineMapper.updateById(saleWeightMachine);

        //修改车牌号
        List<FinishedProductBeOutOfStorage> result = finishedProductBeOutOfStorageMapper.selectList(new EntityWrapper<>(
                new FinishedProductBeOutOfStorage()
                        .setMachineId(saleWeightMachine.getMachineId())
                        .setStatus(Status.TRUE.getKey())));
        if(!ObjectUtils.isEmpty(result)){
            result.stream().forEach(obj->{
                obj.setLicensePlateNumber(trainNumber);
                finishedProductBeOutOfStorageMapper.updateById(obj);
            });
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @Override
    public ResponseResult<WeightMachineWebCountDTO> getWeightMachineWebCount(Integer isWeight, AuthPlatformUserInfo userInfo) {
        List<SaleWeightMachine> saleWeightMachines =  saleWeightMachineMapper.selectList(new EntityWrapper<>(new SaleWeightMachine()
                .setIsWeight(isWeight).setEnterpriseId(userInfo.getOrgId()).setStatus(Status.TRUE.getKey())));
        WeightMachineWebCountDTO weightMachineWebCountDTO = new WeightMachineWebCountDTO(0,0,BigDecimal.ZERO);
        if(ObjectUtils.isEmpty(saleWeightMachines)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,weightMachineWebCountDTO);
        }
        Integer size = saleWeightMachines.size();
        if(EnumWeightMachine.IsWeight.NO.getValue().equals(isWeight)){
            weightMachineWebCountDTO.setWaitWeightCar(size);
        }
        if(EnumWeightMachine.IsWeight.YES.getValue().equals(isWeight)){
            weightMachineWebCountDTO.setAlreadyWeightCar(size);
            BigDecimal weight = BigDecimal.ZERO;
            for (SaleWeightMachine saleWeightMachine : saleWeightMachines) {
                weight = weight.add(saleWeightMachine.getNetWeight());
            }
            weightMachineWebCountDTO.setAlreadyNetWeight(weight);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,weightMachineWebCountDTO);
    }

    @Override
    public List<WeightMachineWebDTO> getWeightMachineWebList(WeightMachineWebListRq rq, AuthPlatformUserInfo userInfo, Pagination pagination) {
        {
            EntityWrapper wrapper = new EntityWrapper();
            wrapper.eq("is_weight", rq.getIsWeight());
            wrapper.eq("enterprise_id", userInfo.getOrgId());
            wrapper.eq("status", Status.TRUE.getKey());
            //车牌号
            if(!ObjectUtils.isEmpty(rq.getTrainNumber())){
                wrapper.like("train_number", rq.getTrainNumber());
            }
            //合同号
            if(!ObjectUtils.isEmpty(rq.getContractNum())){
                wrapper.like("contract_num", rq.getContractNum());
            }

            //带称重时间
            if(EnumWeightMachine.IsWeight.NO.getValue().equals(rq.getIsWeight())){
                if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
                    wrapper.between("create_time",rq.getStartTime() + DateUtils.TIME_SUFFIX ,rq.getEndTime() +  DateUtils.TIME_END );
                }
                if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
                    wrapper.le("create_time",rq.getEndTime() +  DateUtils.TIME_END );
                }
                if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
                    wrapper.ge("create_time",rq.getStartTime() + DateUtils.TIME_SUFFIX);
                }
                wrapper.orderBy("create_time",false);
            }
            //已称重
            if(EnumWeightMachine.IsWeight.YES.getValue().equals(rq.getIsWeight())){
                if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
                    wrapper.between("weighing_time",rq.getStartTime() + DateUtils.TIME_SUFFIX ,rq.getEndTime() +  DateUtils.TIME_END );
                }
                if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
                    wrapper.le("weighing_time",rq.getEndTime() +  DateUtils.TIME_END );
                }
                if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
                    wrapper.ge("weighing_time",rq.getStartTime() + DateUtils.TIME_SUFFIX);
                }
                wrapper.orderBy("weighing_time",false);
            }
            //发货单位
            if(!ObjectUtils.isEmpty(rq.getSupOrCustId())){
                wrapper.eq("receiving_company_id", rq.getSupOrCustId());
            }
            List<WeightMachineWebDTO> result = new ArrayList<>();
            List<SaleWeightMachine> saleWeightMachines = saleWeightMachineMapper.selectPage(pagination, wrapper);
            if(CollectionUtils.isEmpty(saleWeightMachines)){
                return result;
            }
            result = BeanUtils.assemble(WeightMachineWebDTO.class, saleWeightMachines);
            return result;
        }
    }


    /**
     * @Description web根据磅单号获取详细信息
     * @author chenxm66777123
     * @Date 2019/11/25 16:04
     * @version 1.0.0
     */
    @Override
    public ResponseResult<WeightMachineWebDTO> getWeightMachineWebDeatil(String machineId) {
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(machineId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        WeightMachineWebDTO weightMachineWebDTO = BeanUtils.copyProperties(saleWeightMachine, WeightMachineWebDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,weightMachineWebDTO);
    }

    /**
     * @Description web磅单忽略/恢复
     * @author chenxm66777123
     * @Date 2019/11/26 15:55
     * @version 1.0.0
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> ignoreRecoveryMachine(WeightIgnoreMachineRq rq) {
        //查询地磅信息
        SaleWeightMachine saleWeightMachine = saleWeightMachineMapper.selectOne(
                new SaleWeightMachine().setMachineId(rq.getMachineId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(saleWeightMachine)) {
            log.error("根据地磅id查询不到地磅数据！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.SYSTEM_NOT_FOUND);
        }
        saleWeightMachine.setBindIgnore(rq.getIsIgnore());
        saleWeightMachineMapper.updateById(saleWeightMachine);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void batchBindContract(List<Map<String, Object>> param) {
        saleWeightMachineMapper.batchBindContract(param);

        //修改合同号
        for (Map<String, Object> map : param) {
            String machineId =  map.get("machineId").toString();
            String contractNum = map.get("contractNum").toString();
            //修改合同号
            //修改车牌号
            List<FinishedProductBeOutOfStorage> result = finishedProductBeOutOfStorageMapper.selectList(new EntityWrapper<>(
                    new FinishedProductBeOutOfStorage()
                            .setMachineId(machineId)
                            .setStatus(Status.TRUE.getKey())));
            if(!ObjectUtils.isEmpty(result)){
                result.stream().forEach(obj->{
                    obj.setContractId(contractNum);
                    finishedProductBeOutOfStorageMapper.updateById(obj);
                });
            }
        }
    }

    /**
     * @Description 吨转化为kg
     * @author chenxm66777123
     * @Date 2019/10/13 17:31
     * @version 1.0.0
     */
    private BigDecimal tonTokg(BigDecimal data){
        if(ObjectUtils.isEmpty(data)){
            data = BigDecimal.ZERO;
        }
        return data.setScale(3,BigDecimal.ROUND_HALF_UP);
    }

    private BigDecimal bigDecimalIsNull(BigDecimal data){
        if(ObjectUtils.isEmpty(data)){
            return  BigDecimal.ZERO;
        }
        if(data.compareTo(BigDecimal.ZERO) == 0){
            return  BigDecimal.ZERO;
        }
        return data.setScale(3,BigDecimal.ROUND_HALF_UP);
    }
}
