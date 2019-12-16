package com.bee.platform.cloud.si.manufacture.service.manufacturebuy.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.BuyCarrierTransportSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.WeightMachineRq;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyCarrierTransportDetailService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyCarrierTransportService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyTransportSectionService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyWeightMachineService;
import com.bee.platform.common.constants.enums.EnumLogistics;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.AuthSupplierNameDTO;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumGenerateIdModule;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.GenerateIdService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.SupplierInfoUtils;
import io.netty.util.internal.StringUtil;
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
import java.util.Map;

/**
 * <p>
 * 承运方运输表(采购) 服务实现类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
@Slf4j
@Service
public class BuyCarrierTransportServiceImpl extends ServiceImpl<BuyCarrierTransportMapper, BuyCarrierTransport> implements BuyCarrierTransportService {

    @Autowired
    private BuyCarrierTransportMapper buyCarrierTransportMapper;

    @Autowired
    private BuyCarrierTransportDetailMapper buyCarrierTransportDetailMapper;

    @Autowired
    private BuyCarrierTransportDetailService buyCarrierTransportDetailService;

    @Autowired
    private BuyTransportSectionService buyTransportSectionService;

    @Autowired
    private BuyWeightMachineService weightMachineService;

    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;

    @Autowired
    private BuyLogisticsBatchMapper buyLogisticsBatchMapper;

    @Autowired
    private BuyTransportSectionMapper buyTransportSectionMapper;

    @Autowired
    private ConfigLocationMapper configLocationMapper;

    @Autowired
    private GenerateIdService generateIdService;

    @Autowired
    private SupplierInfoUtils supplierInfoUtils;

    /**
     * 保存承运方运输段信息
     * @param carrierTransportDTO
     * @param userInfo
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal saveCarrierTransportInfo(BuyCarrierTransportDTO carrierTransportDTO, AuthPlatformUserInfo userInfo) {

        //批次运量
        BigDecimal freightVolume = BigDecimal.ZERO;
        //承运方运量
        BigDecimal carrierFreightVolume = BigDecimal.ZERO;

        BuyCarrierTransport carrierTransport;
        //判断是否是新增承运商
        if (!StringUtils.isEmpty(carrierTransportDTO.getCarrierTransportId())) {
            carrierTransport = buyCarrierTransportMapper.selectOne(new BuyCarrierTransport()
                    .setCarrierTransportId(carrierTransportDTO.getCarrierTransportId()).setStatus(Status.TRUE.getKey()));
        } else {
            carrierTransport = BeanUtils.copyProperties(carrierTransportDTO, BuyCarrierTransport.class);
            //查询承运商名称
            if (!StringUtils.isEmpty(carrierTransport.getCarrierId())) {
                AuthSupplierNameDTO supplierNameDTO = supplierInfoUtils.getSupplierInfo(carrierTransport.getCarrierId().intValue());
                if (!ObjectUtils.isEmpty(supplierNameDTO)) {
                    carrierTransport.setCarrierName(supplierNameDTO.getName());
                }
            }

            //运输段承运方id
            String carrierTransportId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.CARRIER_TRANSPORT.getKey());
            carrierTransport.setCarrierTransportId(carrierTransportId);
        }

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(carrierTransportDTO.getContractBusinessId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_CAN_FIND_DATA);
        }

        //查询批次信息
        BuyLogisticsBatch logisticsBatch = buyLogisticsBatchMapper.selectOne(new BuyLogisticsBatch()
                .setBatchId(carrierTransportDTO.getBatchId()));
        if (ObjectUtils.isEmpty(logisticsBatch) || ObjectUtils.isEmpty(logisticsBatch.getId())) {
            log.error("未查询到批次信息！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.BATCH_ID_CAN_FIND_DATA);
        }

        //保存车次相关信息，并返回车次总运量
        if (!CollectionUtils.isEmpty(carrierTransportDTO.getDetailDTOS())) {
            for (BuyTransportDetailDTO detailDTO : carrierTransportDTO.getDetailDTOS()) {
                if (ObjectUtils.isEmpty(detailDTO.getCargoWeight())) {
                    detailDTO.setCargoWeight(BigDecimal.ZERO);
                }
                BuyCarrierTransportDetail carrierTransportDetail = BeanUtils.copyProperties(detailDTO, BuyCarrierTransportDetail.class);
                //运输段承运方id
                carrierTransportDetail.setCarrierTransportId(carrierTransport.getCarrierTransportId());
                //运输段id
                carrierTransportDetail.setTransportSectionId(carrierTransport.getTransportSectionId());
                //合同业务id
                carrierTransportDetail.setContractBusinessId(carrierTransport.getContractBusinessId());
                //批次id
                carrierTransportDetail.setBatchId(carrierTransport.getBatchId());
                //运输模式
                carrierTransportDetail.setTransportMode(carrierTransport.getTransportMode());
                carrierTransportDetail.setStatus(Status.TRUE.getKey());
                carrierTransportDetail.setCreateId(userInfo.getId());
                carrierTransportDetail.setCreator(userInfo.getName());
                carrierTransportDetail.setCreateTime(new Date());

                carrierFreightVolume = carrierFreightVolume.add(carrierTransportDetail.getCargoWeight());


                if (EnumLogistics.to_factory.YES.getKey().equals(carrierTransportDTO.getToFactory())
                        && EnumLogistics.transport_mode.car.getKey().equals(carrierTransportDTO.getTransportMode())) {
                    //运量
                    freightVolume = freightVolume.add(carrierTransportDetail.getCargoWeight());
                    if (Status.TRUE.getKey().equals(logisticsBatch.getStatus())) {
                        //如果批次已经确认，并且是到厂的汽运方式，则需要推送信息到磅房
                        WeightMachineRq buyWeightMachineRq = new WeightMachineRq();
                        buyWeightMachineRq.setEnterpriseId(contractBasic.getEnterpriseId());
                        buyWeightMachineRq.setFactoryId(contractBasic.getFactoryId());
                        buyWeightMachineRq.setContractBusinessId(contractBasic.getContractBusinessId());
                        buyWeightMachineRq.setContractNum(contractBasic.getContractNum());
                        buyWeightMachineRq.setBatchId(carrierTransportDetail.getBatchId());
                        buyWeightMachineRq.setTrainNumber(carrierTransportDetail.getTrainNumber());
                        String driver = ObjectUtils.isEmpty(carrierTransportDetail.getDriver()) ? StringUtil.EMPTY_STRING : carrierTransportDetail.getDriver();
                        String contact = ObjectUtils.isEmpty(carrierTransportDetail.getContact()) ? StringUtil.EMPTY_STRING : carrierTransportDetail.getContact();
                        buyWeightMachineRq.setDriver(driver);
                        buyWeightMachineRq.setContact(contact);
                        buyWeightMachineRq.setCarrierId(carrierTransport.getCarrierId().intValue());
                        buyWeightMachineRq.setCarrierName(carrierTransport.getCarrierName());
                        buyWeightMachineRq.setDeliveryCompany(contractBasic.getSupplierName());
                        buyWeightMachineRq.setReceivingCompany(userInfo.getOrg_name());
                        buyWeightMachineRq.setProductId(contractBasic.getProductId());
                        buyWeightMachineRq.setProductName(contractBasic.getProductName());
                        buyWeightMachineRq.setCargoWeight(carrierTransportDetail.getCargoWeight());
                        ResponseResult<String> result = weightMachineService
                                .saveBuyWeightMachine(buyWeightMachineRq, userInfo, EnumWeightMachine.DataSource.LOGISTICS_PUSH.getValue());
                        //车次绑定榜单业务id
                        carrierTransportDetail.setMachineId(result.getObject());
                    }
                }

                if (buyCarrierTransportDetailMapper.insert(carrierTransportDetail) <=0 ) {
                    log.error("保存承运商车次情况失败！");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_CARRIER_TRANSPORT_DETAIL_FAILED);
                }
            }
        }

        if (!StringUtils.isEmpty(carrierTransportDTO.getCarrierTransportId())) {
            carrierTransport.setFreightVolume(carrierTransport.getFreightVolume().add(carrierFreightVolume));
            carrierTransport.setModifyId(userInfo.getId());
            carrierTransport.setModifier(userInfo.getName());
            carrierTransport.setModifyTime(new Date());
            if (buyCarrierTransportMapper.updateById(carrierTransport) <= 0) {
                log.error("保存运输段承运商情况失败！");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_CARRIER_TRANSPORT_FAILED);
            }
        } else {
            carrierTransport.setFreightVolume(carrierFreightVolume);
            carrierTransport.setStatus(Status.TRUE.getKey());
            carrierTransport.setCreateId(userInfo.getId());
            carrierTransport.setCreator(userInfo.getName());
            carrierTransport.setCreateTime(new Date());
            if (buyCarrierTransportMapper.insert(carrierTransport) <= 0) {
                log.error("保存运输段承运商情况失败！");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_CARRIER_TRANSPORT_FAILED);
            }
        }

        return carrierFreightVolume;
    }

    /**
     * 根据运输段id查询运输段承运方信息
     * @param transportSectionId
     * @return
     */
    @Override
    public List<BuyCarrierTransportDTO> getCarrierTransportByBatch(String transportSectionId) {

        List<BuyCarrierTransportDTO> carrierTransportDTOS = new ArrayList<>();

        //根据运输段id查询承运方运输段信息
        List<BuyCarrierTransport> carrierTransports = buyCarrierTransportMapper.selectList(
                new EntityWrapper<BuyCarrierTransport>().eq("transport_section_id", transportSectionId)
                        .eq("status", Status.TRUE.getKey())
                        .orderBy("carrier_transport_id", true));
        if (CollectionUtils.isEmpty(carrierTransports)) {
            return carrierTransportDTOS;
        }

        //遍历承运方运输段信息,查询运输车次信息
        for (BuyCarrierTransport carrierTransport : carrierTransports) {
            //处理承运方运输段信息
            BuyCarrierTransportDTO carrierTransportDTO = BeanUtils.copyProperties(carrierTransport, BuyCarrierTransportDTO.class);

            //根据承运方运输段id查询运输车次信息
            List<BuyTransportDetailDTO> detailDTOS = buyCarrierTransportDetailService
                    .getTransportDetailByTransportId(carrierTransportDTO.getCarrierTransportId());
            carrierTransportDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(carrierTransportDTO.getTransportMode()));
            carrierTransportDTO.setToFactoryName(EnumLogistics.to_factory.getValue(carrierTransportDTO.getToFactory()));
            carrierTransportDTO.setDetailDTOS(detailDTOS);
            carrierTransportDTOS.add(carrierTransportDTO);
        }

        return carrierTransportDTOS;
    }

    /**
     * 根据运输段id查询到厂运输段承运方信息
     * @param transportSectionId
     * @return
     */
    @Override
    public List<BuyCarrierTransportDTO> getToFactoryCarrierTransport(String transportSectionId) {

        List<BuyCarrierTransportDTO> carrierTransportDTOS = new ArrayList<>();

        //根据运输段id查询到厂承运方运输段信息
        List<BuyCarrierTransport> carrierTransports = buyCarrierTransportMapper.selectList(
                new EntityWrapper<BuyCarrierTransport>().eq("transport_section_id", transportSectionId)
                        .eq("status", Status.TRUE.getKey())
                        .eq("to_factory", EnumLogistics.to_factory.YES.getKey())
                        .orderBy("carrier_transport_id", true));
        if (CollectionUtils.isEmpty(carrierTransports)) {
            return carrierTransportDTOS;
        }

        //遍历承运方运输段信息,查询运输车次信息
        for (BuyCarrierTransport carrierTransport : carrierTransports) {
            //处理承运方运输段信息
            BuyCarrierTransportDTO carrierTransportDTO = BeanUtils.copyProperties(carrierTransport, BuyCarrierTransportDTO.class);

            //根据承运方运输段id查询运输车次信息
            List<BuyTransportDetailDTO> detailDTOS = buyCarrierTransportDetailService
                    .getTransportDetailByTransportId(carrierTransportDTO.getCarrierTransportId());
            carrierTransportDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(carrierTransportDTO.getTransportMode()));
            carrierTransportDTO.setToFactoryName(EnumLogistics.to_factory.getValue(carrierTransportDTO.getToFactory()));
            carrierTransportDTO.setDetailDTOS(detailDTOS);
            carrierTransportDTOS.add(carrierTransportDTO);
        }

        return carrierTransportDTOS;
    }

    /**
     * 单独保存运输段承运方信息
     * @param carrierTransportDTO
     * @param userInfo
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveCarrierTransport(BuyCarrierTransportDTO carrierTransportDTO, AuthPlatformUserInfo userInfo) {

        if (StringUtils.isEmpty(carrierTransportDTO.getContractBusinessId())
                || StringUtils.isEmpty(carrierTransportDTO.getBatchId())) {
            log.error("合同业务id为空或者批次id为空，不能保存！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        //保存运输段承运方信息
        BigDecimal freightVolume = saveCarrierTransportInfo(carrierTransportDTO, userInfo);

        //更新批次的运量和合同的在途量
        return buyTransportSectionService.updateContractFreightVolume(carrierTransportDTO.getContractBusinessId(),
                carrierTransportDTO.getBatchId(), freightVolume, userInfo);
    }

    /**
     * 将批次中的到厂车辆推送至磅房
     * @param contractBasic
     * @param logisticsBatch
     * @param userInfo
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void pushCarInfoToWeight(BuyContractBasic contractBasic, BuyLogisticsBatch logisticsBatch, AuthPlatformUserInfo userInfo) {

        //查询批次中到厂的车辆信息
        List<BuyCarrierTransportDetail> transportDetails = buyCarrierTransportDetailMapper
                .getToFactoryCarInfo(contractBasic.getContractBusinessId(), logisticsBatch.getBatchId());

        //遍历车辆数据，推送至磅房
        if (!CollectionUtils.isEmpty(transportDetails)) {
            for (BuyCarrierTransportDetail detail : transportDetails) {
                //查询承运方信息
                BuyCarrierTransport carrierTransport = buyCarrierTransportMapper.selectOne(
                        new BuyCarrierTransport().setCarrierTransportId(detail.getCarrierTransportId()));
                WeightMachineRq buyWeightMachineRq = new WeightMachineRq();
                buyWeightMachineRq.setEnterpriseId(contractBasic.getEnterpriseId());
                buyWeightMachineRq.setFactoryId(contractBasic.getFactoryId());
                buyWeightMachineRq.setContractBusinessId(contractBasic.getContractBusinessId());
                buyWeightMachineRq.setContractNum(contractBasic.getContractNum());
                buyWeightMachineRq.setBatchId(detail.getBatchId());
                buyWeightMachineRq.setTrainNumber(detail.getTrainNumber());
                String driver = ObjectUtils.isEmpty(detail.getDriver()) ? StringUtil.EMPTY_STRING : detail.getDriver();
                String contact = ObjectUtils.isEmpty(detail.getContact()) ? StringUtil.EMPTY_STRING : detail.getContact();
                buyWeightMachineRq.setDriver(driver);
                buyWeightMachineRq.setContact(contact);
                buyWeightMachineRq.setCarrierId(carrierTransport.getCarrierId().intValue());
                buyWeightMachineRq.setCarrierName(carrierTransport.getCarrierName());
                buyWeightMachineRq.setDeliveryCompany(contractBasic.getSupplierName());
                buyWeightMachineRq.setReceivingCompany(userInfo.getOrg_name());
                buyWeightMachineRq.setProductId(contractBasic.getProductId());
                buyWeightMachineRq.setProductName(contractBasic.getProductName());
                buyWeightMachineRq.setCargoWeight(detail.getCargoWeight());
                ResponseResult<String> result = weightMachineService
                        .saveBuyWeightMachine(buyWeightMachineRq, userInfo, EnumWeightMachine.DataSource.LOGISTICS_PUSH.getValue());
                //车次绑定榜单业务id
                detail.setMachineId(result.getObject());
                buyCarrierTransportDetailMapper.updateById(detail);
            }
        }
    }

    /**
     * 根据承运方查询运输段承运方信息
     * @param pagination
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<List<BuyCarrierTransportDTO>> getCarrierTransportByCarrier(Pagination pagination, AuthPlatformUserInfo userInfo) {

        List<BuyCarrierTransportDTO> carrierTransportDTOS = new ArrayList<>();

        //根据承运方名称查询承运方运输段信息
        List<BuyCarrierTransport> carrierTransports = buyCarrierTransportMapper.getTransportInfoByCarrier(userInfo.getOrg_name(), pagination);
        if (!CollectionUtils.isEmpty(carrierTransports)) {
            //遍历承运方运输段信息,查询运输车次信息
            for (BuyCarrierTransport carrierTransport : carrierTransports) {
                //处理承运方运输段信息
                BuyCarrierTransportDTO carrierTransportDTO = BeanUtils.copyProperties(carrierTransport, BuyCarrierTransportDTO.class);
                //根据承运方运输段id查询运输车次信息
                List<BuyTransportDetailDTO> detailDTOS = buyCarrierTransportDetailService
                        .getTransportDetailByTransportId(carrierTransportDTO.getCarrierTransportId());
                carrierTransportDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(carrierTransportDTO.getTransportMode()));
                carrierTransportDTO.setToFactoryName(EnumLogistics.to_factory.getValue(carrierTransportDTO.getToFactory()));
                carrierTransportDTO.setDetailDTOS(detailDTOS);
                carrierTransportDTOS.add(carrierTransportDTO);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, carrierTransportDTOS, PageUtils.transToPage(pagination));
    }

    /**
     * 根据承运方id查询承运方相关运输段信息
     * @param carrierTransportId
     * @return
     */
    @Override
    public ResponseResult<BuyCarrierInfoDTO> getCarrierInfoByTransportId(String carrierTransportId) {

        BuyCarrierInfoDTO carrierInfoDTO = new BuyCarrierInfoDTO();

        //根据承运方id查询承运方信息
        BuyCarrierTransport carrierTransport = buyCarrierTransportMapper.selectOne(
                new BuyCarrierTransport().setCarrierTransportId(carrierTransportId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(carrierTransport) || ObjectUtils.isEmpty(carrierTransport.getId())) {
            log.error("根据承运方id查询承运方信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        //处理承运方运输段信息
        BuyCarrierTransportDTO carrierTransportDTO = BeanUtils.copyProperties(carrierTransport, BuyCarrierTransportDTO.class);
        //根据承运方运输段id查询运输车次信息
        List<BuyTransportDetailDTO> detailDTOS = buyCarrierTransportDetailService
                .getTransportDetailByTransportId(carrierTransportDTO.getCarrierTransportId());
        carrierTransportDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(carrierTransportDTO.getTransportMode()));
        carrierTransportDTO.setToFactoryName(EnumLogistics.to_factory.getValue(carrierTransportDTO.getToFactory()));
        carrierTransportDTO.setDetailDTOS(detailDTOS);
        carrierInfoDTO.setCarrierTransportDTO(carrierTransportDTO);

        //查询当前运输段信息
        BuyTransportSection transportSection = buyTransportSectionMapper.selectOne(
                new BuyTransportSection().setTransportSectionId(carrierTransport.getTransportSectionId())
                        .setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(transportSection) || ObjectUtils.isEmpty(transportSection.getId())) {
            log.error("根据运输段id查询运输段信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        //处理运输段信息
        BuyTransportSectionDTO transportSectionDTO = BeanUtils.copyProperties(transportSection, BuyTransportSectionDTO.class);
        transportSectionDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(transportSectionDTO.getTransportMode()));
        carrierInfoDTO.setTransportSectionInfo(transportSectionDTO);

        //查询上一个运输段信息
        if (transportSection.getTransportSection() > 1) {
            //有上一个运输段时
            BuyTransportSection lastTransportSection = buyTransportSectionMapper.selectOne(
                    new BuyTransportSection().setContractBusinessId(carrierTransport.getContractBusinessId()).setBatchId(carrierTransport.getBatchId())
                            .setTransportSection(transportSection.getTransportSection() - 1).setStatus(Status.TRUE.getKey()));
            if (ObjectUtils.isEmpty(lastTransportSection) || ObjectUtils.isEmpty(lastTransportSection.getId())) {
                log.error("查询上一个运输段信息失败！");
                return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
            }
            //处理运输段信息
            BuyTransportSectionDTO lastTransportSectionDTO = BeanUtils.copyProperties(lastTransportSection, BuyTransportSectionDTO.class);
            //查询运输段承运商信息
            List<BuyCarrierTransportDTO> carrierTransportDTOS = this.getCarrierTransportByBatch(
                    transportSectionDTO.getTransportSectionId());
            lastTransportSectionDTO.setCarrierTransportDTOS(carrierTransportDTOS);
            lastTransportSectionDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(lastTransportSectionDTO.getTransportMode()));
            carrierInfoDTO.setLastTransportSectionInfo(lastTransportSectionDTO);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, carrierInfoDTO);
    }

    /**
     * 承运方保存运输段承运方信息
     * @param carrierTransportDTO
     * @param userInfo
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveCarrierTransportSection(BuyCarrierTransportDTO carrierTransportDTO, AuthPlatformUserInfo userInfo) {

        if (StringUtils.isEmpty(carrierTransportDTO.getContractBusinessId())
                || StringUtils.isEmpty(carrierTransportDTO.getBatchId())) {
            log.error("合同业务id为空或者批次id为空，不能保存！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (!StringUtils.isEmpty(carrierTransportDTO.getId())) {
            log.error("运输段承运方的id不为空，已经存在，不能再次保存！");
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.CARRIER_TRANSPORT_ID_HAS_EXIST);
        }

        //批次运量
        BigDecimal freightVolume = BigDecimal.ZERO;
        //承运方运量
        BigDecimal carrierFreightVolume = BigDecimal.ZERO;

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(carrierTransportDTO.getContractBusinessId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_CAN_FIND_DATA);
        }

        //查询批次信息
        BuyLogisticsBatch logisticsBatch = buyLogisticsBatchMapper.selectOne(new BuyLogisticsBatch()
                .setBatchId(carrierTransportDTO.getBatchId()));
        if (ObjectUtils.isEmpty(logisticsBatch) || ObjectUtils.isEmpty(logisticsBatch.getId())) {
            log.error("未查询到批次信息！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.BATCH_ID_CAN_FIND_DATA);
        }

        //根据承运方id查询承运方信息
        BuyCarrierTransport carrierTransport = buyCarrierTransportMapper.selectOne(
                new BuyCarrierTransport().setCarrierTransportId(carrierTransportDTO.getCarrierTransportId())
                        .setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(carrierTransport) || ObjectUtils.isEmpty(carrierTransport.getId())) {
            log.error("根据承运方id查询承运方信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        carrierTransport.setUnitPrice(carrierTransportDTO.getUnitPrice());
        carrierTransport.setCarriage(carrierTransportDTO.getCarriage());
        carrierTransport.setDepartureTime(carrierTransportDTO.getDepartureTime());
        carrierTransport.setEstimateArrivalTime(carrierTransportDTO.getEstimateArrivalTime());

        //删除现存的承运商相关的车次信息
        buyCarrierTransportDetailMapper.delete(new EntityWrapper<BuyCarrierTransportDetail>().eq("carrier_transport_id", carrierTransport.getCarrierTransportId()));

        //保存车次相关信息，并返回车次总运量
        if (!CollectionUtils.isEmpty(carrierTransportDTO.getDetailDTOS())) {
            for (BuyTransportDetailDTO detailDTO : carrierTransportDTO.getDetailDTOS()) {
                BuyCarrierTransportDetail carrierTransportDetail = BeanUtils.copyProperties(detailDTO, BuyCarrierTransportDetail.class);
                //运输段承运方id
                carrierTransportDetail.setCarrierTransportId(carrierTransport.getCarrierTransportId());
                //运输段id
                carrierTransportDetail.setTransportSectionId(carrierTransport.getTransportSectionId());
                //合同业务id
                carrierTransportDetail.setContractBusinessId(carrierTransport.getContractBusinessId());
                //批次id
                carrierTransportDetail.setBatchId(carrierTransport.getBatchId());
                //运输模式
                carrierTransportDetail.setTransportMode(carrierTransport.getTransportMode());
                carrierTransportDetail.setStatus(Status.TRUE.getKey());
                carrierTransportDetail.setCreateId(userInfo.getId());
                carrierTransportDetail.setCreator(userInfo.getName());
                carrierTransportDetail.setCreateTime(new Date());

                carrierFreightVolume = carrierFreightVolume.add(carrierTransportDetail.getCargoWeight());

                if (EnumLogistics.to_factory.YES.getKey().equals(carrierTransportDTO.getToFactory())
                        && EnumLogistics.transport_mode.car.getKey().equals(carrierTransportDTO.getTransportMode())) {
                    //运量
                    freightVolume = freightVolume.add(carrierTransportDetail.getCargoWeight());

                    if (Status.TRUE.getKey().equals(logisticsBatch.getStatus())) {
                        //如果批次已经确认，并且是到厂的汽运方式，则需要推送信息到磅房
                        WeightMachineRq buyWeightMachineRq = new WeightMachineRq();
                        buyWeightMachineRq.setEnterpriseId(contractBasic.getEnterpriseId());
                        buyWeightMachineRq.setFactoryId(contractBasic.getFactoryId());
                        buyWeightMachineRq.setContractBusinessId(contractBasic.getContractBusinessId());
                        buyWeightMachineRq.setContractNum(contractBasic.getContractNum());
                        buyWeightMachineRq.setBatchId(carrierTransportDetail.getBatchId());
                        buyWeightMachineRq.setTrainNumber(carrierTransportDetail.getTrainNumber());
                        String driver = ObjectUtils.isEmpty(carrierTransportDetail.getDriver()) ? StringUtil.EMPTY_STRING : carrierTransportDetail.getDriver();
                        String contact = ObjectUtils.isEmpty(carrierTransportDetail.getContact()) ? StringUtil.EMPTY_STRING : carrierTransportDetail.getContact();
                        buyWeightMachineRq.setDriver(driver);
                        buyWeightMachineRq.setContact(contact);
                        buyWeightMachineRq.setCarrierId(carrierTransport.getCarrierId().intValue());
                        buyWeightMachineRq.setCarrierName(carrierTransport.getCarrierName());
                        buyWeightMachineRq.setReceivingCompany(userInfo.getOrg_name());
                        buyWeightMachineRq.setDeliveryCompany(contractBasic.getSupplierName());
                        buyWeightMachineRq.setProductId(contractBasic.getProductId());
                        buyWeightMachineRq.setProductName(contractBasic.getProductName());
                        buyWeightMachineRq.setCargoWeight(carrierTransportDetail.getCargoWeight());
                        ResponseResult<String> result = weightMachineService
                                .saveBuyWeightMachine(buyWeightMachineRq, userInfo, EnumWeightMachine.DataSource.LOGISTICS_PUSH.getValue());
                        //车次绑定榜单业务id
                        carrierTransportDetail.setMachineId(result.getObject());
                    }
                }

                if (buyCarrierTransportDetailMapper.insert(carrierTransportDetail) <=0 ) {
                    log.error("保存承运商车次情况失败！");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_CARRIER_TRANSPORT_DETAIL_FAILED);
                }
            }
        }

        carrierTransport.setFreightVolume(carrierFreightVolume);
        carrierTransport.setStatus(Status.TRUE.getKey());
        carrierTransport.setCreateId(userInfo.getId());
        carrierTransport.setCreator(userInfo.getName());
        carrierTransport.setCreateTime(new Date());
        if (buyCarrierTransportMapper.updateById(carrierTransport) <= 0) {
            log.error("保存运输段承运商情况失败！");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_CARRIER_TRANSPORT_FAILED);
        }

        //更新批次的运量和合同的在途量
        return buyTransportSectionService.updateContractFreightVolume(carrierTransportDTO.getContractBusinessId(),
                carrierTransportDTO.getBatchId(), freightVolume, userInfo);
    }

    /**
     * 查询承运方相关的地点信息
     * @param carrierTransport
     */
    private void selectLocateInfo(BuyCarrierTransport carrierTransport) {
        if (carrierTransport.getStartingPlaceId() != null) {
            ConfigLocation configLocation = configLocationMapper.selectById(carrierTransport.getStartingPlaceId());
            if (!ObjectUtils.isEmpty(configLocation)) {
                carrierTransport.setStartingPlace(configLocation.getName());
            }
        }
        if (carrierTransport.getArrivalPlaceId() != null) {
            ConfigLocation configLocation = configLocationMapper.selectById(carrierTransport.getArrivalPlaceId());
            if (!ObjectUtils.isEmpty(configLocation)) {
                carrierTransport.setArrivalPlace(configLocation.getName());
            }
        }
    }

    /**
     * 根据条件查采购运输台账
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    @Override
    public ResponseResult<List<BuyTransportReportDTO>> getCarrierTransportBy(AuthPlatformUserInfo userInfo,
                                                                             BuyCarrierTransportSearchRQ rq, Pagination pagination){
        Map<String, Object> param = JSON.parseObject(JSON.toJSONString(rq));
        param.put("enterpriseId", userInfo.getOrgId());
        param.put("factoryId", userInfo.getFactoryId());
        List<BuyTransportReportDTO> reportDTOS = buyCarrierTransportMapper.getBuyLogisticsReportForm(param, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, reportDTOS, PageUtils.transToPage(pagination));
    }

}
