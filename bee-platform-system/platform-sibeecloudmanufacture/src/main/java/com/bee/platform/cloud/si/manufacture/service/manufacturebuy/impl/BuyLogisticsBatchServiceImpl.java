package com.bee.platform.cloud.si.manufacture.service.manufacturebuy.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BuyContractBasicMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BuyLogisticsBatchMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BuyWeightMachineMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.BuyContractBasic;
import com.bee.platform.cloud.si.manufacture.entity.BuyLogisticsBatch;
import com.bee.platform.cloud.si.manufacture.entity.BuyTransportSection;
import com.bee.platform.cloud.si.manufacture.rq.LogisticsContractListSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.SaveLogisticsRq;
import com.bee.platform.cloud.si.manufacture.rq.SaveTransportSectionRq;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyCarrierTransportService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyLogisticsBatchService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuySampleService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyTransportSectionService;
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
import com.bee.platform.common.utils.DateUtils;
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
import java.util.*;

/**
 * <p>
 * 物流批次表(采购) 服务实现类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
@Slf4j
@Service
public class BuyLogisticsBatchServiceImpl extends ServiceImpl<BuyLogisticsBatchMapper, BuyLogisticsBatch> implements BuyLogisticsBatchService {

    @Autowired
    private BuyLogisticsBatchMapper buyLogisticsBatchMapper;

    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;

    @Autowired
    private BuyWeightMachineMapper buyWeightMachineMapper;

    @Autowired
    private BuyTransportSectionService buyTransportSectionService;

    @Autowired
    private BuyCarrierTransportService buyCarrierTransportService;

    @Autowired
    private GenerateIdService generateIdService;

    @Autowired
    private BuySampleService sampleService;

    /**
     * 根据合同业务id查询物流信息
     * @param contractBusinessId
     * @return
     */
    @Override
    public BuyLogisticsInfoDTO getLogisticsBatchList(String contractBusinessId) {

        BuyLogisticsInfoDTO logisticsInfoDTO = new BuyLogisticsInfoDTO();
        List<BuyLogisticsBatchDTO> logisticsBatchDTOS = new ArrayList<>();

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_IS_EMPTY);
        }
        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_CAN_FIND_DATA);
        }
        //处理合同信息
        BuyContractListContentDTO contractInfoDTO = BeanUtils.copyProperties(contractBasic, BuyContractListContentDTO.class);
        logisticsInfoDTO.setContractInfoDTO(contractInfoDTO);

        //根据合同业务id查询批次信息
        List<BuyLogisticsBatch> logisticsBatches = buyLogisticsBatchMapper.selectList(new EntityWrapper<BuyLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (CollectionUtils.isEmpty(logisticsBatches)) {
            return logisticsInfoDTO;
        }

        //遍历批次信息，查询相关信息
        for (BuyLogisticsBatch logisticsBatch : logisticsBatches) {
            //处理批次信息
            BuyLogisticsBatchDTO logisticsBatchDTO = BeanUtils.copyProperties(logisticsBatch, BuyLogisticsBatchDTO.class);

            //根据批次id查询运输段信息
            List<BuyTransportSectionDTO> transportSectionDTOS = buyTransportSectionService.getTransportSectionInfo(logisticsBatchDTO.getBatchId());

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
    public ResponseResult<BuyLogisticsBatchDTO> getLogisticsContractInfo(String contractBusinessId, AuthPlatformUserInfo userInfo) {

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        //处理合同信息
        BuyContractListContentDTO contractInfoDTO = BeanUtils.copyProperties(contractBasic, BuyContractListContentDTO.class);

        //新增批次信息
        BuyLogisticsBatch buyLogisticsBatch = new BuyLogisticsBatch();
        //批次id
        String batchId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.LOGISTICS_BATCH.getKey());
        buyLogisticsBatch.setBatchId(batchId);
        //批次名称
        //查询合同下的批次数量
        Integer logisticsCount = getLogisticsCount(contractBusinessId);
        logisticsCount = ObjectUtils.isEmpty(logisticsCount) ? 1 : logisticsCount+1;
        buyLogisticsBatch.setBatchName("第" + StringUtil.changeNum(logisticsCount) + "批次");
        //合同业务id
        buyLogisticsBatch.setContractBusinessId(contractBasic.getContractBusinessId());
        //数量初始化设为0
        buyLogisticsBatch.setFreightVolume(BigDecimal.ZERO);
        buyLogisticsBatch.setArrivalVolume(BigDecimal.ZERO);
        buyLogisticsBatch.setStatus(Status.TRUE.getKey());
        buyLogisticsBatch.setCreateId(userInfo.getId());
        buyLogisticsBatch.setCreator(userInfo.getName());
        buyLogisticsBatch.setCreateTime(new Date());
        if (buyLogisticsBatchMapper.insert(buyLogisticsBatch) <= 0) {
            log.error("保存批次情况失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        BuyLogisticsBatchDTO logisticsBatchDTO = BeanUtils.copyProperties(buyLogisticsBatch, BuyLogisticsBatchDTO.class);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, logisticsBatchDTO);
    }

    /**
     * 查询合同批次阶段信息
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<BuyNewLogisticsDTO> getLogisticsContractDetail(String contractBusinessId, AuthPlatformUserInfo userInfo) {

        BuyNewLogisticsDTO buyNewLogisticsDTO = new BuyNewLogisticsDTO();

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        //处理合同信息
        BuyContractListContentDTO contractInfoDTO = BeanUtils.copyProperties(contractBasic, BuyContractListContentDTO.class);
        buyNewLogisticsDTO.setContractInfoDTO(contractInfoDTO);

        //根据合同业务id查询批次信息
        List<BuyLogisticsBatch> logisticsBatches = buyLogisticsBatchMapper.selectList(new EntityWrapper<BuyLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (!CollectionUtils.isEmpty(logisticsBatches)) {
            List<BuyLogisticsBatchDTO> logisticsBatchDTOS = BeanUtils.assemble(BuyLogisticsBatchDTO.class, logisticsBatches);
            //遍历批次信息，查询相关信息
            for (BuyLogisticsBatchDTO logisticsBatch : logisticsBatchDTOS) {

                //根据批次id查询运输段信息
                List<BuyTransportSection> transportSections = buyTransportSectionService.selectList(
                        new EntityWrapper<BuyTransportSection>().eq("batch_id", logisticsBatch.getBatchId())
                                .eq("status", Status.TRUE.getKey()).orderBy("transport_section", true));
                if (!CollectionUtils.isEmpty(transportSections)) {
                    List<BuyTransportSectionDTO> transportSectionDTOS = BeanUtils.assemble(BuyTransportSectionDTO.class, transportSections);
                    logisticsBatch.setTransportSectionDTOS(transportSectionDTOS);
                }
            }
            buyNewLogisticsDTO.setLogisticsBatchDTOS(logisticsBatchDTOS);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, buyNewLogisticsDTO);
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
        BuyLogisticsBatch logisticsBatch = buyLogisticsBatchMapper.selectOne(new BuyLogisticsBatch()
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
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(logisticsBatch.getContractBusinessId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，确认物流批次信息失败！");
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
        if (buyLogisticsBatchMapper.updateById(logisticsBatch) <= 0) {
            log.error("保存批次情况失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        contractBasic.setModifyId(userInfo.getId());
        contractBasic.setModifier(userInfo.getName());
        contractBasic.setModifyTime(new Date());
        if (buyContractBasicMapper.updateById(contractBasic) <= 0) {
            log.error("更新合同信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //将批次中的到厂车辆推送至磅房
        buyCarrierTransportService.pushCarInfoToWeight(contractBasic, logisticsBatch, userInfo);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 查询合同详情中的物流批次信息
     * @param contractBusinessId
     * @return
     */
    @Override
    public ResponseResult<List<BuyContractBatchDTO>> getContractBatchInfo(String contractBusinessId) {

        List<BuyContractBatchDTO> contractBatchDTOS = new ArrayList<>();

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_IS_EMPTY);
        }

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流批次失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }

        //根据合同业务id查询批次信息
        List<BuyLogisticsBatch> logisticsBatches = buyLogisticsBatchMapper.selectList(new EntityWrapper<BuyLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (CollectionUtils.isEmpty(logisticsBatches)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contractBatchDTOS);
        }

        //遍历批次信息，处理
        for (BuyLogisticsBatch logisticsBatch : logisticsBatches) {
            BuyContractBatchDTO contractBatchDTO = BeanUtils.copyProperties(logisticsBatch, BuyContractBatchDTO.class);
            //查询最早出厂时间
            contractBatchDTO.setDepartureTime(buyLogisticsBatchMapper.selectBatchDepartureTime(logisticsBatch.getBatchId()));
            //查询最早称重时间
            contractBatchDTO.setWeightTime(buyLogisticsBatchMapper.selectBatchWeightTime(logisticsBatch.getBatchId()));
            //查询到厂货运方式
            String transportMode = buyLogisticsBatchMapper.selectBatchTransportMode(logisticsBatch.getBatchId());
            if (!ObjectUtils.isEmpty(transportMode)) {
                transportMode = transportMode.replace(EnumLogistics.transport_mode.car.getKey().toString(), EnumLogistics.transport_mode.car.getValue());
                transportMode = transportMode.replace(EnumLogistics.transport_mode.boat.getKey().toString(), EnumLogistics.transport_mode.boat.getValue());
                transportMode = transportMode.replace(EnumLogistics.transport_mode.train.getKey().toString(), EnumLogistics.transport_mode.train.getValue());
            }
            contractBatchDTO.setTransportMode(transportMode);
            //查询相关车辆信息
            List<BuyCarSampleDTO> carSampleDTOS = sampleService.getCarSimple(contractBasic.getContractBusinessId(), logisticsBatch.getBatchId());
            contractBatchDTO.setCarSampleDTOS(carSampleDTOS);
            //查询批次到货数量
            contractBatchDTO.setArrivalVolume(buyWeightMachineMapper.getArrivalVolumeByBatchId(logisticsBatch.getBatchId()));

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
    public ResponseResult<List<BuyContractListContentDTO>> getLogisticsContractList(String keyword, Integer purchaserMode,
                                                                       Pagination pagination, AuthPlatformUserInfo userInfo) {

        List<BuyContractListContentDTO> contents = new ArrayList<>();
        List<BuyContractBasic> contracts = buyContractBasicMapper.selectPage(pagination,new EntityWrapper<BuyContractBasic>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("purchaser_mode", purchaserMode)
                .like("contract_num", keyword)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("completed", 0)
                .orderBy("create_time", false));
        if(!CollectionUtils.isEmpty(contracts)){
            contents = BeanUtils.assemble(BuyContractListContentDTO.class, contracts);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contents, PageUtils.transToPage(pagination));
    }

    /**
     * 物流人员查询物流记录
     * @param pagination
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<List<ContractLogisticInfoDTO>> getContractLogisticInfoList(Pagination pagination, AuthPlatformUserInfo userInfo) {

        //查询完成的合同信息(采购、销售)
        List<ContractLogisticInfoDTO> logisticInfoDTOS = buyLogisticsBatchMapper.getContractLogisticInfo(userInfo.getOrgId(), pagination);

        if (!CollectionUtils.isEmpty(logisticInfoDTOS)) {
            for (ContractLogisticInfoDTO logisticInfo : logisticInfoDTOS) {
                if (logisticInfo.getBusinessType() == 1) {
                    //采购
                    //出厂时间
                    logisticInfo.setDepartureTime(buyLogisticsBatchMapper.selectBuyContractDepartureTime(logisticInfo.getContractBusinessId()));
                    //到货时间
                    logisticInfo.setArrivalTime(buyLogisticsBatchMapper.selectBuyContractDepartureTime(logisticInfo.getContractBusinessId()));
                } else {
                    //销售
                    //出厂时间
                    logisticInfo.setDepartureTime(buyLogisticsBatchMapper.selectSaleContractDepartureTime(logisticInfo.getContractBusinessId()));
                    //到货时间
                    logisticInfo.setArrivalTime(buyLogisticsBatchMapper.selectSaleContractArrivalTime(logisticInfo.getContractBusinessId()));
                }
            }
        }


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, logisticInfoDTOS, PageUtils.transToPage(pagination));
    }

    /**
     * 分页查询物流订单列表
     * @param rq
     * @param pagination
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<List<LogisticsContractListContentDTO>> getAllLogisticsContractList(LogisticsContractListSearchRQ rq,
                                                                                             Pagination pagination, AuthPlatformUserInfo userInfo) {

        Map<String, Object> params = new HashMap<>(10);
        params.put("contractNum", rq.getContractNum());
        params.put("productId", rq.getProductId());
        if (!ObjectUtils.isEmpty(rq.getBusinessType()) && rq.getBusinessType() != 0) {
            params.put("businessType", rq.getBusinessType());
        }
        if(!ObjectUtils.isEmpty(rq.getStartTime())){
            rq.setStartTime(rq.getStartTime() + DateUtils.TIME_SUFFIX);
            params.put("startTime", rq.getStartTime());
        }
        if(!ObjectUtils.isEmpty(rq.getEndTime())){
            rq.setEndTime(rq.getEndTime() + DateUtils.TIME_END);
            params.put("endTime", rq.getEndTime());
        }
        params.put("sort", rq.getSort());
        params.put("enterpriseId", userInfo.getOrgId());
        params.put("factoryId", userInfo.getFactoryId());
        params.put("enterpriseName", userInfo.getOrg_name());
        List<LogisticsContractListContentDTO> logisticsListContentDTOS = buyLogisticsBatchMapper.getAllLogisticsList(params, pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, logisticsListContentDTOS, PageUtils.transToPage(pagination));
    }

    /**
     * 根据合同业务id查询物流批次阶段信息
     * @param contractBusinessId
     * @return
     */
    @Override
    public BuyLogisticsInfoDTO getLogisticsBatchSection(String contractBusinessId) {

        BuyLogisticsInfoDTO logisticsInfoDTO = new BuyLogisticsInfoDTO();
        List<BuyLogisticsBatchDTO> logisticsBatchDTOS = new ArrayList<>();

        //校验合同业务id
        if (StringUtils.isEmpty(contractBusinessId)) {
            log.error("合同业务id为空，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_IS_EMPTY);
        }
        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询物流信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_CAN_FIND_DATA);
        }
        //处理合同信息
        BuyContractListContentDTO contractInfoDTO = BeanUtils.copyProperties(contractBasic, BuyContractListContentDTO.class);
        logisticsInfoDTO.setContractInfoDTO(contractInfoDTO);

        //根据合同业务id查询批次信息
        List<BuyLogisticsBatch> logisticsBatches = buyLogisticsBatchMapper.selectList(new EntityWrapper<BuyLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (CollectionUtils.isEmpty(logisticsBatches)) {
            return logisticsInfoDTO;
        }

        //遍历批次信息，查询相关信息
        for (BuyLogisticsBatch logisticsBatch : logisticsBatches) {
            //处理批次信息
            BuyLogisticsBatchDTO logisticsBatchDTO = BeanUtils.copyProperties(logisticsBatch, BuyLogisticsBatchDTO.class);

            //根据批次id查询运输段信息
            List<BuyTransportSectionDTO> transportSectionDTOS = buyTransportSectionService.getTransportSectionInfoByBatchId(logisticsBatchDTO.getBatchId());

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

        BuyLogisticsBatch logisticsBatch = new BuyLogisticsBatch();

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
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
            String batchId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.LOGISTICS_BATCH.getKey());
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
            if (buyLogisticsBatchMapper.insert(logisticsBatch) <= 0) {
                log.error("保存批次情况失败！");
                return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
            }
        } else {
            //存在，查询批次信息
            logisticsBatch = buyLogisticsBatchMapper.selectOne(new BuyLogisticsBatch()
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
                buyTransportSectionService.saveTransportSection(sectionRq, logisticsBatch, userInfo);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 查询合同的第一个批次id
     * @param contractBusinessId
     * @return
     */
    @Override
    public String getContractLogisticsBatchId(String contractBusinessId) {

        //根据合同业务id查询批次信息
        List<BuyLogisticsBatch> logisticsBatches = buyLogisticsBatchMapper.selectList(new EntityWrapper<BuyLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (CollectionUtils.isEmpty(logisticsBatches)) {
            return null;
        } else {
            return logisticsBatches.get(0).getBatchId();
        }
    }

    /**
     * 查询合同下的批次数量
     * @param contractBusinessId
     * @return
     */
    private Integer getLogisticsCount(String contractBusinessId) {
        //查询合同下的批次数量
        return buyLogisticsBatchMapper.selectCount(new EntityWrapper<BuyLogisticsBatch>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", Status.TRUE.getKey()));
    }

}
