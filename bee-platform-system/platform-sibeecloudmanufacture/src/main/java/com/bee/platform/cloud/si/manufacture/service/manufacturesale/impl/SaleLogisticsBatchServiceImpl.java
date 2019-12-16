package com.bee.platform.cloud.si.manufacture.service.manufacturesale.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleCarrierTransportDetailMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleCarrierTransportMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleContractBasicMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleLogisticsBatchMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.SaveLogisticsRq;
import com.bee.platform.cloud.si.manufacture.rq.SaveTransportSectionRq;
import com.bee.platform.cloud.si.manufacture.rq.WeightMachineRq;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleCarrierTransportDetailService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleLogisticsBatchService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleTransportSectionService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleWeightMachineService;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumLogistics;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumGenerateIdModule;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.GenerateIdService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 物流批次表(销售) 服务实现类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
@Slf4j
@Service
public class SaleLogisticsBatchServiceImpl extends ServiceImpl<SaleLogisticsBatchMapper, SaleLogisticsBatch> implements SaleLogisticsBatchService {

    @Autowired
    private SaleLogisticsBatchMapper saleLogisticsBatchMapper;

    @Autowired
    private SaleContractBasicMapper saleContractBasicMapper;

    @Autowired
    private SaleTransportSectionService saleTransportSectionService;

    @Autowired
    private SaleWeightMachineService saleWeightMachineService;

    @Autowired
    private SaleCarrierTransportDetailMapper saleCarrierTransportDetailMapper;

    @Autowired
    private SaleCarrierTransportMapper saleCarrierTransportMapper;

    @Autowired
    private GenerateIdService generateIdService;

    /**
     * 根据合同业务id查询物流信息
     * @param contractBusinessId
     * @return
     */
    @Override
    public SaleLogisticsInfoDTO getLogisticsBatchList(String contractBusinessId) {

        SaleLogisticsInfoDTO logisticsInfoDTO = new SaleLogisticsInfoDTO();
        List<SaleLogisticsBatchDTO> logisticsBatchDTOS = new ArrayList<>();

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_IS_EMPTY);
        }
        //查询合同信息
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_CAN_FIND_DATA);
        }
        //处理合同信息
        SaleContractListContentDTO contractInfoDTO = BeanUtils.copyProperties(contractBasic, SaleContractListContentDTO.class);
        logisticsInfoDTO.setContractInfoDTO(contractInfoDTO);

        //根据合同业务id查询批次信息
        List<SaleLogisticsBatch> logisticsBatches = saleLogisticsBatchMapper.selectList(new EntityWrapper<SaleLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (CollectionUtils.isEmpty(logisticsBatches)) {
            return logisticsInfoDTO;
        }

        //遍历批次信息，查询相关信息
        for (SaleLogisticsBatch logisticsBatch : logisticsBatches) {
            //处理批次信息
            SaleLogisticsBatchDTO logisticsBatchDTO = BeanUtils.copyProperties(logisticsBatch, SaleLogisticsBatchDTO.class);

            //根据批次id查询运输段信息
            List<SaleTransportSectionDTO> transportSectionDTOS = saleTransportSectionService.getTransportSectionInfo(logisticsBatchDTO.getBatchId());

            logisticsBatchDTO.setTransportSectionDTOS(transportSectionDTOS);
            logisticsBatchDTOS.add(logisticsBatchDTO);
        }
        logisticsInfoDTO.setLogisticsBatchDTOS(logisticsBatchDTOS);

        return logisticsInfoDTO;
    }

    /**
     * 查询新增批次合同信息
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<SaleLogisticsBatchDTO> getLogisticsContractInfo(String contractBusinessId, AuthPlatformUserInfo userInfo) {

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        //查询合同信息
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }

        //新增批次信息
        SaleLogisticsBatch logisticsBatch = new SaleLogisticsBatch();
        //批次id
        String batchId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.SALE_LOGISTICS_BATCH.getKey());
        logisticsBatch.setBatchId(batchId);
        //批次名称
        //查询合同下的批次数量
        Integer logisticsCount = getLogisticsCount(contractBusinessId);
        logisticsCount = ObjectUtils.isEmpty(logisticsCount) ? 1 : logisticsCount+1;
        logisticsBatch.setBatchName("第" + StringUtil.changeNum(logisticsCount) + "批次");
        //合同业务id
        logisticsBatch.setContractBusinessId(contractBasic.getContractBusinessId());
        //数量初始化设为0
        logisticsBatch.setFreightVolume(BigDecimal.ZERO);
        logisticsBatch.setArrivalVolume(BigDecimal.ZERO);
        logisticsBatch.setStatus(Status.TRUE.getKey());
        logisticsBatch.setCreateId(userInfo.getId());
        logisticsBatch.setCreator(userInfo.getName());
        logisticsBatch.setCreateTime(new Date());
        if (saleLogisticsBatchMapper.insert(logisticsBatch) <= 0) {
            log.error("保存批次情况失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        SaleLogisticsBatchDTO logisticsBatchDTO = BeanUtils.copyProperties(logisticsBatch, SaleLogisticsBatchDTO.class);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, logisticsBatchDTO);
    }

    /**
     * 查询合同批次阶段信息
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<SaleNewLogisticsDTO> getLogisticsContractDetail(String contractBusinessId, AuthPlatformUserInfo userInfo) {

        SaleNewLogisticsDTO saleNewLogisticsDTO = new SaleNewLogisticsDTO();

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        //查询合同信息
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        //处理合同信息
        SaleContractListContentDTO contractInfoDTO = BeanUtils.copyProperties(contractBasic, SaleContractListContentDTO.class);
        saleNewLogisticsDTO.setContractInfoDTO(contractInfoDTO);

        //根据合同业务id查询批次信息
        List<SaleLogisticsBatch> logisticsBatches = saleLogisticsBatchMapper.selectList(new EntityWrapper<SaleLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (!CollectionUtils.isEmpty(logisticsBatches)) {
            List<SaleLogisticsBatchDTO> logisticsBatchDTOS = BeanUtils.assemble(SaleLogisticsBatchDTO.class, logisticsBatches);
            //遍历批次信息，查询相关信息
            for (SaleLogisticsBatchDTO logisticsBatch : logisticsBatchDTOS) {

                //根据批次id查询运输段信息
                List<SaleTransportSection> transportSections = saleTransportSectionService.selectList(
                        new EntityWrapper<SaleTransportSection>().eq("batch_id", logisticsBatch.getBatchId())
                                .eq("status", Status.TRUE.getKey()).orderBy("transport_section", true));
                if (!CollectionUtils.isEmpty(transportSections)) {
                    List<SaleTransportSectionDTO> transportSectionDTOS = BeanUtils.assemble(SaleTransportSectionDTO.class, transportSections);
                    logisticsBatch.setTransportSectionDTOS(transportSectionDTOS);
                }
            }
            saleNewLogisticsDTO.setLogisticsBatchDTOS(logisticsBatchDTOS);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleNewLogisticsDTO);
    }

    /**
     * 确认物流批次信息
     * @param batchId
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveLogisticsBatchInfo(String batchId, AuthPlatformUserInfo userInfo) {

        //查询批次信息
        SaleLogisticsBatch logisticsBatch = saleLogisticsBatchMapper.selectOne(new SaleLogisticsBatch()
                .setBatchId(batchId));
        if (ObjectUtils.isEmpty(logisticsBatch) || ObjectUtils.isEmpty(logisticsBatch.getId())) {
            log.error("未查询到批次信息，保存失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        if (Status.TRUE.getKey().equals(logisticsBatch.getStatus())) {
            log.error("当前批次信息已确认，不能再次确认！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //查询合同信息
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(logisticsBatch.getContractBusinessId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到销售合同信息，确认物流批次信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        //更新合同在途量
        contractBasic.setTrafficVolume(contractBasic.getTrafficVolume().add(logisticsBatch.getFreightVolume()));

        //查询合同下的批次数量
        Integer logisticsCount = getLogisticsCount(contractBasic.getContractBusinessId());
        logisticsCount = ObjectUtils.isEmpty(logisticsCount) ? 1 : logisticsCount+1;
        //确定批次名称
        logisticsBatch.setBatchName("第" + StringUtil.changeNum(logisticsCount) + "批次");
        //确认批次，将批次置为有效
        logisticsBatch.setStatus(Status.TRUE.getKey());
        logisticsBatch.setCreateId(userInfo.getId());
        logisticsBatch.setCreator(userInfo.getName());
        logisticsBatch.setCreateTime(new Date());
        if (saleLogisticsBatchMapper.updateById(logisticsBatch) <= 0) {
            log.error("保存批次情况失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        contractBasic.setModifyId(userInfo.getId());
        contractBasic.setModifier(userInfo.getName());
        contractBasic.setModifyTime(new Date());
        if (saleContractBasicMapper.updateById(contractBasic) <= 0) {
            log.error("更新合同信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //查询批次中第一段的汽运车辆信息
        List<SaleCarrierTransportDetail> carInfos = saleCarrierTransportDetailMapper.getFirstSectionCarInfo(batchId);
        //将批次中第一段的车辆推送至出库
        if (!CollectionUtils.isEmpty(carInfos)) {
            for (SaleCarrierTransportDetail carInfo : carInfos) {
                //查询承运方信息
                SaleCarrierTransport carrierTransport = saleCarrierTransportMapper.selectOne(
                        new SaleCarrierTransport().setCarrierTransportId(carInfo.getCarrierTransportId()));
                WeightMachineRq saleWeightMachineRq = new WeightMachineRq();
                saleWeightMachineRq.setFactoryId(contractBasic.getFactoryId());
                saleWeightMachineRq.setContractBusinessId(contractBasic.getContractBusinessId());
                saleWeightMachineRq.setContractNum(contractBasic.getContractNum());
                saleWeightMachineRq.setTrainNumber(carInfo.getTrainNumber());
                String driver = ObjectUtils.isEmpty(carInfo.getDriver()) ? io.netty.util.internal.StringUtil.EMPTY_STRING : carInfo.getDriver();
                String contact = ObjectUtils.isEmpty(carInfo.getContact()) ? io.netty.util.internal.StringUtil.EMPTY_STRING : carInfo.getContact();
                saleWeightMachineRq.setDriver(driver);
                saleWeightMachineRq.setContact(contact);
                saleWeightMachineRq.setCarrierId(carrierTransport.getCarrierId().intValue());
                saleWeightMachineRq.setCarrierName(carrierTransport.getCarrierName());
                saleWeightMachineRq.setDeliveryCompany(userInfo.getOrg_name());
                saleWeightMachineRq.setReceivingCompany(contractBasic.getCustomerName());
                saleWeightMachineRq.setProductId(contractBasic.getProductId());
                saleWeightMachineRq.setProductName(contractBasic.getProductName());
                saleWeightMachineRq.setCargoWeight(carInfo.getCargoWeight());
                saleWeightMachineService.saveSaleWeightMachine(saleWeightMachineRq, userInfo, EnumWeightMachine.DataSource.LOGISTICS_PUSH.getValue());
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 查询合同详情中的物流批次信息
     * @param contractBusinessId
     * @return
     */
    @Override
    public ResponseResult<List<SaleContractBatchDTO>> getContractBatchInfo(String contractBusinessId) {

        List<SaleContractBatchDTO> contractBatchDTOS = new ArrayList<>();

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_IS_EMPTY);
        }

        //查询合同信息
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流批次失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }

        //根据合同业务id查询批次信息
        List<SaleLogisticsBatch> logisticsBatches = saleLogisticsBatchMapper.selectList(new EntityWrapper<SaleLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (CollectionUtils.isEmpty(logisticsBatches)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contractBatchDTOS);
        }

        //遍历批次信息，处理
        for (SaleLogisticsBatch logisticsBatch : logisticsBatches) {
            SaleContractBatchDTO contractBatchDTO = BeanUtils.copyProperties(logisticsBatch, SaleContractBatchDTO.class);
            //查询最早出厂时间
            contractBatchDTO.setDepartureTime(saleLogisticsBatchMapper.selectBatchDepartureTime(logisticsBatch.getBatchId()));
            //查询到厂货运方式
            String transportMode = saleLogisticsBatchMapper.selectBatchTransportMode(logisticsBatch.getBatchId());
            if (!StringUtils.isEmpty(transportMode)) {
                transportMode = transportMode.replace(EnumLogistics.transport_mode.car.getKey().toString(), EnumLogistics.transport_mode.car.getValue());
                transportMode = transportMode.replace(EnumLogistics.transport_mode.boat.getKey().toString(), EnumLogistics.transport_mode.boat.getValue());
                transportMode = transportMode.replace(EnumLogistics.transport_mode.train.getKey().toString(), EnumLogistics.transport_mode.train.getValue());
                contractBatchDTO.setTransportMode(transportMode);
            }
            //查询到厂相关车辆信息
            List<SaleCarrierTransportDetail> details = saleCarrierTransportDetailMapper.getToFactoryCarInfo(logisticsBatch.getBatchId());
            List<SaleTransportDetailDTO> transportDetailDTOS = BeanUtils.assemble(SaleTransportDetailDTO.class, details);
            contractBatchDTO.setTransportDetailDTOS(transportDetailDTOS);

            contractBatchDTOS.add(contractBatchDTO);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contractBatchDTOS);
    }

    /**
     * 运输合同列表
     * @param keyword
     * @param purchaserMode
     * @param userInfo
     * @param pagination
     * @return
     */
    @Override
    public ResponseResult<List<SaleContractListContentDTO>> getLogisticsContractList(String keyword, Integer purchaserMode,
                                                                                    Pagination pagination, AuthPlatformUserInfo userInfo) {

        List<SaleContractListContentDTO> contents = new ArrayList<>();
        List<SaleContractBasic> contracts = saleContractBasicMapper.selectPage(pagination,new EntityWrapper<SaleContractBasic>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("sale_mode", purchaserMode)
                .like("contract_num", keyword)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("completed", 0)
                .orderBy("create_time", false));
        if(!CollectionUtils.isEmpty(contracts)){
            contents = BeanUtils.assemble(SaleContractListContentDTO.class, contracts);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contents, PageUtils.transToPage(pagination));
    }

    /**
     * 根据合同业务id查询物流批次阶段信息
     * @param contractBusinessId
     * @return
     */
    @Override
    public SaleLogisticsInfoDTO getLogisticsBatchSection(String contractBusinessId) {

        SaleLogisticsInfoDTO logisticsInfoDTO = new SaleLogisticsInfoDTO();
        List<SaleLogisticsBatchDTO> logisticsBatchDTOS = new ArrayList<>();

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_IS_EMPTY);
        }
        //查询合同信息
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_CAN_FIND_DATA);
        }
        //处理合同信息
        SaleContractListContentDTO contractInfoDTO = BeanUtils.copyProperties(contractBasic, SaleContractListContentDTO.class);
        logisticsInfoDTO.setContractInfoDTO(contractInfoDTO);

        //根据合同业务id查询批次信息
        List<SaleLogisticsBatch> logisticsBatches = saleLogisticsBatchMapper.selectList(new EntityWrapper<SaleLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (CollectionUtils.isEmpty(logisticsBatches)) {
            return logisticsInfoDTO;
        }

        //遍历批次信息，查询相关信息
        for (SaleLogisticsBatch logisticsBatch : logisticsBatches) {
            //处理批次信息
            SaleLogisticsBatchDTO logisticsBatchDTO = BeanUtils.copyProperties(logisticsBatch, SaleLogisticsBatchDTO.class);

            //根据批次id查询运输段信息
            List<SaleTransportSectionDTO> transportSectionDTOS = saleTransportSectionService.getTransportSectionInfoByBatchId(logisticsBatchDTO.getBatchId());

            logisticsBatchDTO.setTransportSectionDTOS(transportSectionDTOS);
            logisticsBatchDTOS.add(logisticsBatchDTO);
        }
        logisticsInfoDTO.setLogisticsBatchDTOS(logisticsBatchDTOS);

        return logisticsInfoDTO;
    }

    /**
     * 新增物流批次阶段信息
     * @param rq
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<String> saveLogisticsSectionInfo(SaveLogisticsRq rq, AuthPlatformUserInfo userInfo) {

        SaleLogisticsBatch logisticsBatch = new SaleLogisticsBatch();

        //查询合同信息
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(rq.getContractBusinessId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，保存物流批次信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }

        //判断批次是否存在
        if (StringUtils.isEmpty(rq.getBatchId())) {
            //不存在，新增批次
            logisticsBatch.setContractBusinessId(rq.getContractBusinessId());
            //批次id
            String batchId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.SALE_LOGISTICS_BATCH.getKey());
            logisticsBatch.setBatchId(batchId);
            //查询合同下的批次数量
            Integer logisticsCount = getLogisticsCount(contractBasic.getContractBusinessId());
            logisticsCount = ObjectUtils.isEmpty(logisticsCount) ? 1 : logisticsCount+1;
            //确定批次名称
            logisticsBatch.setBatchName("第" + StringUtil.changeNum(logisticsCount) + "批次");
            logisticsBatch.setFreightVolume(BigDecimal.ZERO);
            logisticsBatch.setArrivalVolume(BigDecimal.ZERO);
            logisticsBatch.setStatus(Status.TRUE.getKey());
            logisticsBatch.setCreateId(userInfo.getId());
            logisticsBatch.setCreator(userInfo.getName());
            logisticsBatch.setCreateTime(new Date());
            if (saleLogisticsBatchMapper.insert(logisticsBatch) <= 0) {
                log.error("保存批次情况失败！");
                return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
            }
        } else {
            //存在，查询批次信息
            logisticsBatch = saleLogisticsBatchMapper.selectOne(new SaleLogisticsBatch()
                    .setBatchId(rq.getBatchId()).setStatus(Status.TRUE.getKey()));
            if (ObjectUtils.isEmpty(logisticsBatch) || ObjectUtils.isEmpty(logisticsBatch.getId())) {
                log.error("未查询到批次信息，保存失败！");
                return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
            }
        }

        //保存批次的阶段信息
        List<SaveTransportSectionRq> transportSectionDTOS = rq.getTransportSectionDTOS();
        if (!CollectionUtils.isEmpty(transportSectionDTOS)) {
            for (SaveTransportSectionRq sectionRq : transportSectionDTOS) {
                saleTransportSectionService.saveTransportSection(sectionRq, logisticsBatch, userInfo);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 查询合同下的批次数量
     * @param contractBusinessId
     * @return
     */
    private Integer getLogisticsCount(String contractBusinessId) {
        //查询合同下的批次数量
        return saleLogisticsBatchMapper.selectCount(new EntityWrapper<SaleLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey()));
    }

}
