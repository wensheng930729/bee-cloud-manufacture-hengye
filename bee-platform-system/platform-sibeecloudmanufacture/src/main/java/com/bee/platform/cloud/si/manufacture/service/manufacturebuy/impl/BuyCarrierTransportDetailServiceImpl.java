package com.bee.platform.cloud.si.manufacture.service.manufacturebuy.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.BuyTransportDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.WeightMachineRq;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyCarrierTransportDetailService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyTransportSectionService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyWeightMachineService;
import com.bee.platform.common.constants.enums.EnumLogistics;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
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
 * 承运方运输详情表(采购) 服务实现类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
@Slf4j
@Service
public class BuyCarrierTransportDetailServiceImpl extends ServiceImpl<BuyCarrierTransportDetailMapper, BuyCarrierTransportDetail> implements BuyCarrierTransportDetailService {

    @Autowired
    private BuyCarrierTransportDetailMapper buyCarrierTransportDetailMapper;

    @Autowired
    private BuyTransportSectionService buyTransportSectionService;

    @Autowired
    private BuyWeightMachineService weightMachineService;

    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;

    @Autowired
    private BuyLogisticsBatchMapper buyLogisticsBatchMapper;

    @Autowired
    private BuyCarrierTransportMapper buyCarrierTransportMapper;

    @Autowired
    private BuyWeightMachineMapper weightMachineMapper;

    /**
     * 根据承运方运输段ID查询承运方运输详情车次信息
     * @param carrierTransportId
     * @return
     */
    @Override
    public List<BuyTransportDetailDTO> getTransportDetailByTransportId(String carrierTransportId) {

        List<BuyTransportDetailDTO> detailDTOS = new ArrayList<>();

        //根据承运方运输段ID查询运输车次情况
        List<BuyCarrierTransportDetail> details = buyCarrierTransportDetailMapper.selectList(
                new EntityWrapper<BuyCarrierTransportDetail>()
                        .eq("carrier_transport_id", carrierTransportId)
                        .eq("status", Status.TRUE.getKey()).orderBy("createTime", true));
        if (CollectionUtils.isEmpty(details)) {
            return detailDTOS;
        }
        //处理运输车次情况
        for (BuyCarrierTransportDetail detail : details) {
            BuyTransportDetailDTO detailDTO = BeanUtils.copyProperties(detail, BuyTransportDetailDTO.class);
            detailDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(detailDTO.getTransportMode()));
            detailDTOS.add(detailDTO);
        }

        return detailDTOS;
    }

    /**
     * 更新车次信息
     * @param transportDetailDTO
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveTransportDetailByTransport(BuyTransportDetailDTO transportDetailDTO, AuthPlatformUserInfo userInfo) {

        if (StringUtils.isEmpty(transportDetailDTO.getContractBusinessId())
                || StringUtils.isEmpty(transportDetailDTO.getBatchId())) {
            log.error("合同业务id为空或者批次id为空，不能保存！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(transportDetailDTO.getContractBusinessId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_CAN_FIND_DATA);
        }

        //查询批次信息
        BuyLogisticsBatch logisticsBatch = buyLogisticsBatchMapper.selectOne(new BuyLogisticsBatch()
                .setBatchId(transportDetailDTO.getBatchId()));
        if (ObjectUtils.isEmpty(logisticsBatch) || ObjectUtils.isEmpty(logisticsBatch.getId())) {
            log.error("未查询到批次信息！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.BATCH_ID_CAN_FIND_DATA);
        }

        //查询承运方信息
        BuyCarrierTransport carrierTransport = buyCarrierTransportMapper.selectOne(
                new BuyCarrierTransport().setCarrierTransportId(transportDetailDTO.getCarrierTransportId()));
        if (ObjectUtils.isEmpty(carrierTransport) || ObjectUtils.isEmpty(carrierTransport.getId())) {
            log.error("未查询到承运方信息！");
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.CARRIER_TRANSPORT_ID_CAN_FIND_DATA);
        }

        if (ObjectUtils.isEmpty(transportDetailDTO.getCargoWeight())) {
            transportDetailDTO.setCargoWeight(BigDecimal.ZERO);
        }

        //新增车次
        if (ObjectUtils.isEmpty(transportDetailDTO.getId())) {
            return saveNewDetail(transportDetailDTO, carrierTransport, logisticsBatch, contractBasic, userInfo);
        }

        //查询车次信息
        BuyCarrierTransportDetail carrierTransportDetail = this.selectById(transportDetailDTO.getId());

        //查询车次是否已经称重
        if (!ObjectUtils.isEmpty(carrierTransportDetail.getMachineId())) {
            BuyWeightMachine weightMachine = weightMachineMapper.selectOne(new BuyWeightMachine()
                    .setMachineId(carrierTransportDetail.getMachineId()));
            if (EnumWeightMachine.IsWeight.YES.getValue().equals(weightMachine.getInFactoryWeightIsConfirm())) {
                log.error("车次已经进行进厂称重，不能再进行修改！");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.CARRIER_TRANSPORT_DETAIL_HAS_BEEN_WEIGHT);
            }
            weightMachine.setCargoWeight(transportDetailDTO.getCargoWeight());
            weightMachine.setTrainNumber(transportDetailDTO.getTrainNumber());
            String driver = ObjectUtils.isEmpty(transportDetailDTO.getDriver()) ? StringUtil.EMPTY_STRING : transportDetailDTO.getDriver();
            String contact = ObjectUtils.isEmpty(transportDetailDTO.getContact()) ? StringUtil.EMPTY_STRING : transportDetailDTO.getContact();
            weightMachine.setDriver(driver);
            weightMachine.setContact(contact);
            weightMachineMapper.updateById(weightMachine);
        }

        //历史运量
        BigDecimal oldFreightVolume = carrierTransportDetail.getCargoWeight();

        carrierTransportDetail.setTrainNumber(transportDetailDTO.getTrainNumber());
        carrierTransportDetail.setDriver(transportDetailDTO.getDriver());
        carrierTransportDetail.setContact(transportDetailDTO.getContact());
        carrierTransportDetail.setCargoWeight(transportDetailDTO.getCargoWeight());
        carrierTransportDetail.setModifyId(userInfo.getId());
        carrierTransportDetail.setModifier(userInfo.getName());
        carrierTransportDetail.setModifyTime(new Date());
        if (buyCarrierTransportDetailMapper.updateById(carrierTransportDetail) <= 0) {
            log.error("更新车次信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //保存承运商运量
        carrierTransport.setFreightVolume(carrierTransport.getFreightVolume().subtract(oldFreightVolume).add(transportDetailDTO.getCargoWeight()));
        carrierTransport.setModifyId(userInfo.getId());
        carrierTransport.setModifier(userInfo.getName());
        carrierTransport.setModifyTime(new Date());
        if (buyCarrierTransportMapper.updateById(carrierTransport) <= 0) {
            log.error("保存运输段承运商情况失败！");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_CARRIER_TRANSPORT_FAILED);
        }

        //更新批次的运量和合同的在途量
        return buyTransportSectionService.updateContractFreightVolume(transportDetailDTO.getContractBusinessId(),
                transportDetailDTO.getBatchId(), transportDetailDTO.getCargoWeight().subtract(oldFreightVolume), userInfo);
    }

    /**
     * 新增车次
     * @param transportDetailDTO
     * @param carrierTransport
     * @param logisticsBatch
     * @param contractBasic
     * @param userInfo
     * @return
     */
    private ResponseResult<ResCodeEnum> saveNewDetail(BuyTransportDetailDTO transportDetailDTO, BuyCarrierTransport carrierTransport,
                                                      BuyLogisticsBatch logisticsBatch, BuyContractBasic contractBasic,
                                                      AuthPlatformUserInfo userInfo) {
        //保存车次信息
        BuyCarrierTransportDetail carrierTransportDetail = BeanUtils.copyProperties(transportDetailDTO, BuyCarrierTransportDetail.class);
        carrierTransportDetail.setTransportMode(carrierTransport.getTransportMode());
        carrierTransportDetail.setStatus(Status.TRUE.getKey());
        carrierTransportDetail.setCreateId(userInfo.getId());
        carrierTransportDetail.setCreator(userInfo.getName());
        carrierTransportDetail.setCreateTime(new Date());

        if (EnumLogistics.to_factory.YES.getKey().equals(carrierTransport.getToFactory())
                && EnumLogistics.transport_mode.car.getKey().equals(carrierTransport.getTransportMode())
                && Status.TRUE.getKey().equals(logisticsBatch.getStatus())) {
            //如果批次已经确认，并且是到厂的汽运方式，则需要推送信息到磅房
            WeightMachineRq buyWeightMachineRq = new WeightMachineRq();
            buyWeightMachineRq.setEnterpriseId(contractBasic.getEnterpriseId());
            buyWeightMachineRq.setFactoryId(contractBasic.getFactoryId());
            buyWeightMachineRq.setContractNum(contractBasic.getContractNum());
            buyWeightMachineRq.setContractBusinessId(contractBasic.getContractBusinessId());
            buyWeightMachineRq.setBatchId(carrierTransportDetail.getBatchId());
            buyWeightMachineRq.setTrainNumber(carrierTransportDetail.getTrainNumber());
            String driver = ObjectUtils.isEmpty(carrierTransportDetail.getDriver()) ? StringUtil.EMPTY_STRING : carrierTransportDetail.getDriver();
            String contact = ObjectUtils.isEmpty(carrierTransportDetail.getContact()) ? StringUtil.EMPTY_STRING : carrierTransportDetail.getContact();
            buyWeightMachineRq.setDriver(driver);
            buyWeightMachineRq.setContact(contact);
            buyWeightMachineRq.setCarrierName(carrierTransport.getCarrierName());
            buyWeightMachineRq.setReceivingCompany(contractBasic.getSupplierName());
            buyWeightMachineRq.setDeliveryCompany(userInfo.getOrg_name());
            buyWeightMachineRq.setProductId(contractBasic.getProductId());
            buyWeightMachineRq.setProductName(contractBasic.getProductName());
            buyWeightMachineRq.setCargoWeight(carrierTransportDetail.getCargoWeight());
            ResponseResult<String> result = weightMachineService.saveBuyWeightMachine(
                    buyWeightMachineRq, userInfo,EnumWeightMachine.DataSource.LOGISTICS_PUSH.getValue());
            //车次绑定榜单业务id
            carrierTransportDetail.setMachineId(result.getObject());
        }
        if (buyCarrierTransportDetailMapper.insert(carrierTransportDetail) <= 0) {
            log.error("保存车次信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //保存承运商运量
        carrierTransport.setFreightVolume(carrierTransport.getFreightVolume().add(transportDetailDTO.getCargoWeight()));
        carrierTransport.setModifyId(userInfo.getId());
        carrierTransport.setModifier(userInfo.getName());
        carrierTransport.setModifyTime(new Date());
        if (buyCarrierTransportMapper.updateById(carrierTransport) <= 0) {
            log.error("保存运输段承运商情况失败！");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_CARRIER_TRANSPORT_FAILED);
        }

        //更新批次的运量和合同的在途量
        return buyTransportSectionService.updateContractFreightVolume(transportDetailDTO.getContractBusinessId(),
                transportDetailDTO.getBatchId(), transportDetailDTO.getCargoWeight(), userInfo);
    }

}
