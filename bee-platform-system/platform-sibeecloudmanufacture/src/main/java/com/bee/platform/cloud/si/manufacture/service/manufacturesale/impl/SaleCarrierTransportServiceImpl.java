package com.bee.platform.cloud.si.manufacture.service.manufacturesale.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.FinishedProductBeOutOfStorageDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleCarrierTransportDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleTransportDetailDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleTransportReportDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.SaleCarrierTransportSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.WeightMachineRq;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleCarrierTransportDetailService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleCarrierTransportService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleTransportSectionService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleWeightMachineService;
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
import com.bee.platform.common.utils.SupplierInfoUtils;
import com.bee.platform.common.utils.PageUtils;
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
 * 运输段承运方表(销售) 服务实现类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
@Slf4j
@Service
public class SaleCarrierTransportServiceImpl extends ServiceImpl<SaleCarrierTransportMapper, SaleCarrierTransport> implements SaleCarrierTransportService {

    @Autowired
    private SaleCarrierTransportMapper saleCarrierTransportMapper;

    @Autowired
    private SaleCarrierTransportDetailMapper saleCarrierTransportDetailMapper;

    @Autowired
    private SaleCarrierTransportDetailService saleCarrierTransportDetailService;

    @Autowired
    private SaleTransportSectionService saleTransportSectionService;

    @Autowired
    private SaleTransportSectionMapper saleTransportSectionMapper;

    @Autowired
    private SaleContractBasicMapper saleContractBasicMapper;

    @Autowired
    private SaleLogisticsBatchMapper saleLogisticsBatchMapper;

    @Autowired
    private ConfigLocationMapper configLocationMapper;

    @Autowired
    private SaleWeightMachineService saleWeightMachineService;

    @Autowired
    private GenerateIdService generateIdService;

    @Autowired
    private SupplierInfoUtils supplierInfoUtils;

    /**
     * 保存承运方运输段信息
     * @param carrierTransportDTO
     * @param transportSection
     * @param userInfo
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal saveCarrierTransportInfo(SaleCarrierTransportDTO carrierTransportDTO, Integer transportSection,
                                               AuthPlatformUserInfo userInfo) {

        //批次运量
        BigDecimal freightVolume = BigDecimal.ZERO;
        //承运方运量
        BigDecimal carrierFreightVolume = BigDecimal.ZERO;

        SaleCarrierTransport carrierTransport;
        //判断是否是新增承运商
        if (!StringUtils.isEmpty(carrierTransportDTO.getCarrierTransportId())) {
            carrierTransport = saleCarrierTransportMapper.selectOne(new SaleCarrierTransport()
                    .setCarrierTransportId(carrierTransportDTO.getCarrierTransportId()).setStatus(Status.TRUE.getKey()));
        } else {
            carrierTransport = BeanUtils.copyProperties(carrierTransportDTO, SaleCarrierTransport.class);

            //查询承运商名称
            if (!StringUtils.isEmpty(carrierTransport.getCarrierId())) {
                AuthSupplierNameDTO supplierNameDTO = supplierInfoUtils.getSupplierInfo(carrierTransport.getCarrierId().intValue());
                if (!ObjectUtils.isEmpty(supplierNameDTO)) {
                    carrierTransport.setCarrierName(supplierNameDTO.getName());
                }
            }

            //运输段承运方id
            String carrierTransportId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.SALE_CARRIER_TRANSPORT.getKey());
            carrierTransport.setCarrierTransportId(carrierTransportId);
        }

        //查询合同信息
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(carrierTransportDTO.getContractBusinessId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，保存承运方运输段信息失败！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_CAN_FIND_DATA);
        }

        //查询批次信息
        SaleLogisticsBatch logisticsBatch = saleLogisticsBatchMapper.selectOne(new SaleLogisticsBatch()
                .setBatchId(carrierTransportDTO.getBatchId()));
        if (ObjectUtils.isEmpty(logisticsBatch) || ObjectUtils.isEmpty(logisticsBatch.getId())) {
            log.error("未查询到批次信息！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.BATCH_ID_CAN_FIND_DATA);
        }

        if (ObjectUtils.isEmpty(transportSection)) {
            //查询批次运输段信息
            SaleTransportSection saleTransportSection = saleTransportSectionMapper.selectOne(
                    new SaleTransportSection().setTransportSectionId(carrierTransportDTO.getTransportSectionId())
                            .setStatus(Status.TRUE.getKey()));
            if (ObjectUtils.isEmpty(saleTransportSection) || ObjectUtils.isEmpty(saleTransportSection.getId())) {
                log.error("未查询到批次运输段信息！");
                throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.TRANSPORT_SECTION_ID_CAN_FIND_DATA);
            }
            transportSection = saleTransportSection.getTransportSection();
        }

        //保存车次相关信息，并返回车次总运量
        if (!CollectionUtils.isEmpty(carrierTransportDTO.getDetailDTOS())) {
            for (SaleTransportDetailDTO detailDTO : carrierTransportDTO.getDetailDTOS()) {
                if (ObjectUtils.isEmpty(detailDTO.getCargoWeight())) {
                    detailDTO.setCargoWeight(BigDecimal.ZERO);
                }
                SaleCarrierTransportDetail carrierTransportDetail = BeanUtils.copyProperties(detailDTO, SaleCarrierTransportDetail.class);
                //运输段承运方车次id
                String carrierTransportDetailId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.SALE_CARRIER_TRANSPORT_DETAIL.getKey());
                carrierTransportDetail.setCarrierTransportDetailId(carrierTransportDetailId);
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
                if (saleCarrierTransportDetailMapper.insert(carrierTransportDetail) <=0 ) {
                    log.error("保存承运商车次情况失败！");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_CARRIER_TRANSPORT_DETAIL_FAILED);
                }
                carrierFreightVolume = carrierFreightVolume.add(carrierTransportDetail.getCargoWeight());

                //如果批次已经确认，且是批次第一段的汽运，则将信息推送至出库
                if (transportSection != null && transportSection == 0
                        && EnumLogistics.transport_mode.car.getKey().equals(carrierTransportDTO.getTransportMode())) {

                    //运量
                    freightVolume = freightVolume.add(carrierTransportDetail.getCargoWeight());

                    if (Status.TRUE.getKey().equals(logisticsBatch.getStatus())) {
                        WeightMachineRq saleWeightMachineRq = new WeightMachineRq();
                        saleWeightMachineRq.setFactoryId(contractBasic.getFactoryId());
                        saleWeightMachineRq.setContractBusinessId(contractBasic.getContractBusinessId());
                        saleWeightMachineRq.setContractNum(contractBasic.getContractNum());
                        saleWeightMachineRq.setTrainNumber(carrierTransportDetail.getTrainNumber());
                        String driver = ObjectUtils.isEmpty(carrierTransportDetail.getDriver()) ? StringUtil.EMPTY_STRING : carrierTransportDetail.getDriver();
                        String contact = ObjectUtils.isEmpty(carrierTransportDetail.getContact()) ? StringUtil.EMPTY_STRING : carrierTransportDetail.getContact();
                        saleWeightMachineRq.setDriver(driver);
                        saleWeightMachineRq.setContact(contact);
                        saleWeightMachineRq.setCarrierId(carrierTransport.getCarrierId().intValue());
                        saleWeightMachineRq.setCarrierName(carrierTransport.getCarrierName());
                        saleWeightMachineRq.setDeliveryCompany(userInfo.getOrg_name());
                        saleWeightMachineRq.setReceivingCompany(contractBasic.getCustomerName());
                        saleWeightMachineRq.setProductId(contractBasic.getProductId());
                        saleWeightMachineRq.setProductName(contractBasic.getProductName());
                        saleWeightMachineRq.setCargoWeight(carrierTransportDetail.getCargoWeight());
                        saleWeightMachineService.saveSaleWeightMachine(saleWeightMachineRq, userInfo, EnumWeightMachine.DataSource.LOGISTICS_PUSH.getValue());
                    }
                }
            }
        }

        if (!StringUtils.isEmpty(carrierTransportDTO.getCarrierTransportId())) {
            carrierTransport.setFreightVolume(carrierTransport.getFreightVolume().add(carrierFreightVolume));
            carrierTransport.setModifyId(userInfo.getId());
            carrierTransport.setModifier(userInfo.getName());
            carrierTransport.setModifyTime(new Date());
            if (saleCarrierTransportMapper.updateById(carrierTransport) <= 0) {
                log.error("保存运输段承运商情况失败！");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_CARRIER_TRANSPORT_FAILED);
            }
        } else {
            carrierTransport.setFreightVolume(carrierFreightVolume);
            carrierTransport.setStatus(Status.TRUE.getKey());
            carrierTransport.setCreateId(userInfo.getId());
            carrierTransport.setCreator(userInfo.getName());
            carrierTransport.setCreateTime(new Date());
            if (saleCarrierTransportMapper.insert(carrierTransport) <= 0) {
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
    public List<SaleCarrierTransportDTO> getCarrierTransportByBatch(String transportSectionId) {

        List<SaleCarrierTransportDTO> carrierTransportDTOS = new ArrayList<>();

        //根据运输段id查询承运方运输段信息
        List<SaleCarrierTransport> carrierTransports = saleCarrierTransportMapper.selectList(
                new EntityWrapper<SaleCarrierTransport>().eq("transport_section_id", transportSectionId)
                        .eq("status", Status.TRUE.getKey())
                        .orderBy("carrier_transport_id", true));
        if (CollectionUtils.isEmpty(carrierTransports)) {
            return carrierTransportDTOS;
        }

        //遍历承运方运输段信息,查询运输车次信息
        for (SaleCarrierTransport carrierTransport : carrierTransports) {
            //处理承运方运输段信息
            SaleCarrierTransportDTO carrierTransportDTO = BeanUtils.copyProperties(carrierTransport, SaleCarrierTransportDTO.class);

            //根据承运方运输段id查询运输车次信息
            List<SaleTransportDetailDTO> detailDTOS = saleCarrierTransportDetailService
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
    public ResponseResult<ResCodeEnum> saveCarrierTransport(SaleCarrierTransportDTO carrierTransportDTO, AuthPlatformUserInfo userInfo) {

        if (StringUtils.isEmpty(carrierTransportDTO.getContractBusinessId())
                || StringUtils.isEmpty(carrierTransportDTO.getBatchId())) {
            log.error("合同业务id为空或者批次id为空，不能保存！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        //保存运输段承运方信息
        BigDecimal freightVolume = saveCarrierTransportInfo(carrierTransportDTO, null, userInfo);

        //更新批次的运量和合同的在途量
        return saleTransportSectionService.updateContractFreightVolume(carrierTransportDTO.getContractBusinessId(),
                carrierTransportDTO.getBatchId(), freightVolume, userInfo);
    }

    /**
     * 查询承运方相关的地点信息
     * @param carrierTransport
     */
    private void selectLocateInfo(SaleCarrierTransport carrierTransport) {
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
     * 根据根据客户名,商品名，时间范围查询采购运输台账信息
     * @param userInfo
     * @param rq
     * @param pagination 分页信息
     */
    @Override
    public ResponseResult<List<SaleTransportReportDTO>> getCarrierTransportBy(AuthPlatformUserInfo userInfo,
                                                                              SaleCarrierTransportSearchRQ rq, Pagination pagination){
        Map<String, Object> param = JSON.parseObject(JSON.toJSONString(rq));
        param.put("enterpriseId", userInfo.getOrgId());
        param.put("factoryId", userInfo.getFactoryId());
        List<SaleTransportReportDTO> reportDTOS = saleCarrierTransportMapper.getSaleLogisticsReportForm(param, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, reportDTOS, PageUtils.transToPage(pagination));
    }
}
