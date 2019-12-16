package com.bee.platform.cloud.si.manufacture.service.manufacturesale.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.FinishedProductBeOutOfStorageDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleCarDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleTransportDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumStorage;
import com.bee.platform.cloud.si.manufacture.enums.EnumWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.SaleDiscountTranspoerDetailRq;
import com.bee.platform.cloud.si.manufacture.rq.WeightMachineRq;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleCarrierTransportDetailService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleTransportSectionService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleWeightMachineService;
import com.bee.platform.common.constants.enums.EnumLogistics;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
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

/**
 * <p>
 * 承运方运输详情表(销售) 服务实现类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
@Slf4j
@Service
public class SaleCarrierTransportDetailServiceImpl extends ServiceImpl<SaleCarrierTransportDetailMapper, SaleCarrierTransportDetail> implements SaleCarrierTransportDetailService {


    @Autowired
    private SaleCarrierTransportDetailMapper saleCarrierTransportDetailMapper;

    @Autowired
    private SaleTransportSectionMapper saleTransportSectionMapper;

    @Autowired
    private SaleTransportSectionService saleTransportSectionService;

    @Autowired
    private SaleLogisticsBatchMapper saleLogisticsBatchMapper;

    @Autowired
    private SaleCarrierTransportMapper saleCarrierTransportMapper;

    @Autowired
    private SaleContractBasicMapper saleContractBasicMapper;

    @Autowired
    private StorageService storageService;

    @Autowired
    private SaleWeightMachineService saleWeightMachineService;

    /**
     * 根据承运方运输段ID查询承运方运输详情车次信息
     * @param carrierTransportId
     * @return
     */
    @Override
    public List<SaleTransportDetailDTO> getTransportDetailByTransportId(String carrierTransportId) {

        List<SaleTransportDetailDTO> detailDTOS = new ArrayList<>();

        //根据承运方运输段ID查询运输车次情况
        List<SaleCarrierTransportDetail> details = saleCarrierTransportDetailMapper.selectList(
                new EntityWrapper<SaleCarrierTransportDetail>()
                        .eq("carrier_transport_id", carrierTransportId)
                        .eq("status", Status.TRUE.getKey()).orderBy("createTime", true));
        if (CollectionUtils.isEmpty(details)) {
            return detailDTOS;
        }
        //处理运输车次情况
        for (SaleCarrierTransportDetail detail : details) {
            SaleTransportDetailDTO detailDTO = BeanUtils.copyProperties(detail, SaleTransportDetailDTO.class);
            detailDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(detailDTO.getTransportMode()));
            detailDTOS.add(detailDTO);
        }

        return detailDTOS;
    }

    /**
     * 单独保存车次信息
     * @param transportDetailDTO
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveTransportDetailByTransport(SaleTransportDetailDTO transportDetailDTO, AuthPlatformUserInfo userInfo) {

        if (StringUtils.isEmpty(transportDetailDTO.getContractBusinessId())
                || StringUtils.isEmpty(transportDetailDTO.getBatchId())
                || StringUtils.isEmpty(transportDetailDTO.getTransportSectionId())) {
            log.error("合同业务id为空、批次id为空或者运输段id为空，不能保存！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        //查询批次运输段信息
        SaleTransportSection transportSection = saleTransportSectionMapper.selectOne(
                new SaleTransportSection().setTransportSectionId(transportDetailDTO.getTransportSectionId())
                        .setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(transportSection) || ObjectUtils.isEmpty(transportSection.getId())) {
            log.error("未查询到批次运输段信息！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.TRANSPORT_SECTION_ID_CAN_FIND_DATA);
        }

        //查询合同信息
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(transportSection.getContractBusinessId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到销售合同信息！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }

        //保存车次信息
        SaleCarrierTransportDetail carrierTransportDetail = BeanUtils.copyProperties(transportDetailDTO, SaleCarrierTransportDetail.class);
        carrierTransportDetail.setTransportMode(transportSection.getTransportMode());
        carrierTransportDetail.setStatus(Status.TRUE.getKey());
        carrierTransportDetail.setCreateId(userInfo.getId());
        carrierTransportDetail.setCreator(userInfo.getName());
        carrierTransportDetail.setCreateTime(new Date());
        if (saleCarrierTransportDetailMapper.insert(carrierTransportDetail) <= 0) {
            log.error("保存车次信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //如果是第一段的货运，将信息推送至出库
        if (transportSection.getTransportSection() == 0) {
            //查询承运方信息
            SaleCarrierTransport carrierTransport = saleCarrierTransportMapper.selectOne(
                    new SaleCarrierTransport().setCarrierTransportId(carrierTransportDetail.getCarrierTransportId()));
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

        //更新批次的运量和合同的在途量
        return saleTransportSectionService.updateContractFreightVolume(transportDetailDTO.getContractBusinessId(),
                transportDetailDTO.getBatchId(), transportDetailDTO.getCargoWeight(), userInfo);
    }

    /**
     * 查询批次下未到货的车辆信息
     * @param contractBusinessId
     * @return
     */
    @Override
    public ResponseResult<List<SaleTransportDetailDTO>> getNotArrivalTransportDetail(String contractBusinessId) {

        //查询合同下未到货的到厂车辆信息
        List<SaleCarrierTransportDetail> details = saleCarrierTransportDetailMapper.getNotArrivalToFactoryCarInfo(contractBusinessId);
        List<SaleTransportDetailDTO> transportDetailDTOS = new ArrayList<>();
        if (!CollectionUtils.isEmpty(details)) {
            for (SaleCarrierTransportDetail detail : details) {
                SaleTransportDetailDTO detailDTO = BeanUtils.copyProperties(detail, SaleTransportDetailDTO.class);
                detailDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(detailDTO.getTransportMode()));
                // 添加批次名称
            	SaleLogisticsBatch saleLogisticsBatch = saleLogisticsBatchMapper.selectOne(new SaleLogisticsBatch()
            			.setBatchId(detail.getBatchId())
            			.setStatus(Status.TRUE.getKey()));
            	if(!ObjectUtils.isEmpty(saleLogisticsBatch)) {
            		detailDTO.setBatchName(saleLogisticsBatch.getBatchName());
            	}
                transportDetailDTOS.add(detailDTO);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, transportDetailDTOS);
    }

    /**
     * 保存被折价的车辆单价信息
     * @param rq
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveDiscountTransportDetail(SaleDiscountTranspoerDetailRq rq, AuthPlatformUserInfo userInfo) {

        List<String> carrierTransportDetailIds = rq.getCarrierTransportDetailIds();
        String batchId = null;
        BigDecimal arrivalVolume = BigDecimal.ZERO;

        //遍历处理车辆信息
        if (!CollectionUtils.isEmpty(carrierTransportDetailIds)) {
            for (String carrierTransportDetailId : carrierTransportDetailIds) {
                //根据id查询
                SaleCarrierTransportDetail detail = this.selectOne(new EntityWrapper<SaleCarrierTransportDetail>()
                        .eq("carrier_transport_detail_id", carrierTransportDetailId));
                if (ObjectUtils.isEmpty(detail) || ObjectUtils.isEmpty(detail.getId())) {
                    log.error("根据承运方运输车次ID查询不到数据！");
                    throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.CARRIER_TRANSPORT_DETAIL_ID_CAN_FIND_DATA);
                }
                batchId = detail.getBatchId();
                arrivalVolume = arrivalVolume.add(detail.getCargoWeight());
                detail.setArrivalStatus(EnumLogistics.arrival_status.YES.getKey());
                detail.setDiscountUnitPrice(rq.getDiscountUnitPrice());
                // 添加处理方式
                detail.setHandleType(EnumStorage.ProcessMode.discount_storage.getKey());
                detail.setModifyId(userInfo.getId());
                detail.setModifier(userInfo.getName());
                detail.setModifyTime(new Date());
                if (!this.updateById(detail)) {
                    log.error("更新承运方运输车次信息失败");
                    return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
                }
            }
        }
        //更新批次中的到货量
        if (!org.apache.commons.lang.StringUtils.isEmpty(batchId)) {
            SaleLogisticsBatch logisticsBatch = saleLogisticsBatchMapper.selectOne(new SaleLogisticsBatch().setBatchId(batchId));
            BigDecimal arrivalVolumes = logisticsBatch.getArrivalVolume() != null ? logisticsBatch.getArrivalVolume().add(arrivalVolume) : arrivalVolume;
            logisticsBatch.setArrivalVolume(arrivalVolumes);
            saleLogisticsBatchMapper.updateById(logisticsBatch);

            //更新合同的已收货量
            saleTransportSectionService.updateContractReceivedVolume(logisticsBatch.getContractBusinessId(), arrivalVolume, userInfo);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

	@Override
	public ResponseResult<List<SaleCarDTO>> getCarList(String contractBusinessId, AuthPlatformUserInfo userInfo,
			Pagination pagination) {
		
		//查询合同下收货情况
        List<SaleCarrierTransportDetail> details = saleCarrierTransportDetailMapper.getCarList(contractBusinessId,pagination);
        List<SaleCarDTO> saleCarDTO = new ArrayList<>();
        if (!CollectionUtils.isEmpty(details)) {
            for (SaleCarrierTransportDetail detail : details) {
            	SaleCarDTO detailDTO = BeanUtils.copyProperties(detail, SaleCarDTO.class);
                detailDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(detailDTO.getTransportMode()));
                // 未到厂的车辆收货日期为空
                if(EnumLogistics.arrival_status.NO.getKey().equals(detail.getArrivalStatus())) {
                	detailDTO.setModifyTime(null);
                }
                // 添加批次名称
            	SaleLogisticsBatch saleLogisticsBatch = saleLogisticsBatchMapper.selectOne(new SaleLogisticsBatch()
            			.setBatchId(detail.getBatchId())
            			.setStatus(Status.TRUE.getKey()));
            	if(!ObjectUtils.isEmpty(saleLogisticsBatch)) {
            		detailDTO.setBatchName(saleLogisticsBatch.getBatchName());
            	}
                saleCarDTO.add(detailDTO);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, saleCarDTO, PageUtils.transToPage(pagination));
	}

}
