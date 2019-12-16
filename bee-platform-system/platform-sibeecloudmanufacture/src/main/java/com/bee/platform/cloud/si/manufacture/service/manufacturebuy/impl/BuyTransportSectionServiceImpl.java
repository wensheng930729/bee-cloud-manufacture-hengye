package com.bee.platform.cloud.si.manufacture.service.manufacturebuy.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BuyContractBasicMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BuyLogisticsBatchMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BuyTransportSectionMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigLocationMapper;
import com.bee.platform.cloud.si.manufacture.dto.BuyCarrierTransportDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyTransportSectionDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuyContractBasic;
import com.bee.platform.cloud.si.manufacture.entity.BuyLogisticsBatch;
import com.bee.platform.cloud.si.manufacture.entity.BuyTransportSection;
import com.bee.platform.cloud.si.manufacture.entity.ConfigLocation;
import com.bee.platform.cloud.si.manufacture.rq.SaveTransportSectionRq;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyCarrierTransportService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyTransportSectionService;
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
 * 物流批次运输段表(采购) 服务实现类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-24
 */
@Slf4j
@Service
public class BuyTransportSectionServiceImpl extends ServiceImpl<BuyTransportSectionMapper, BuyTransportSection> implements BuyTransportSectionService {

    @Autowired
    private BuyTransportSectionMapper buyTransportSectionMapper;

    @Autowired
    private BuyCarrierTransportService buyCarrierTransportService;

    @Autowired
    private BuyLogisticsBatchMapper buyLogisticsBatchMapper;

    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;

    @Autowired
    private ConfigLocationMapper configLocationMapper;

    @Autowired
    private GenerateIdService generateIdService;

    /**
     * 根据批次id查询运输段信息
     * @param batchId
     * @return
     */
    @Override
    public List<BuyTransportSectionDTO> getTransportSectionInfo(String batchId) {

        List<BuyTransportSectionDTO> transportSectionDTOS = new ArrayList<>();

        //根据批次id查询运输段信息
        List<BuyTransportSection> transportSections = buyTransportSectionMapper.selectList(
                new EntityWrapper<BuyTransportSection>().eq("batch_id", batchId)
                        .eq("status", Status.TRUE.getKey()).orderBy("transport_section", true));
        if (CollectionUtils.isEmpty(transportSections)) {
            return transportSectionDTOS;
        }

        //遍历运输段信息，处理承运商相关信息
        for (BuyTransportSection transportSection : transportSections) {
            //处理运输段信息
            BuyTransportSectionDTO transportSectionDTO = BeanUtils.copyProperties(transportSection, BuyTransportSectionDTO.class);

            //查询运输段承运商信息
            List<BuyCarrierTransportDTO> carrierTransportDTOS = buyCarrierTransportService
                    .getCarrierTransportByBatch(transportSectionDTO.getTransportSectionId());

            transportSectionDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(transportSectionDTO.getTransportMode()));
            transportSectionDTO.setCarrierTransportDTOS(carrierTransportDTOS);
            transportSectionDTOS.add(transportSectionDTO);
        }

        return transportSectionDTOS;
    }

    /**
     * 根据运输段id查询运输段信息(到厂的承运商)
     * @param transportSectionId
     * @return
     */
    @Override
    public ResponseResult<BuyTransportSectionDTO> getTransportSectionDetail(String transportSectionId) {
        BuyTransportSection transportSection = buyTransportSectionMapper.selectOne(
                new BuyTransportSection().setTransportSectionId(transportSectionId));

        //处理运输段信息
        BuyTransportSectionDTO transportSectionDTO = BeanUtils.copyProperties(transportSection, BuyTransportSectionDTO.class);

        //查询运输段承运商信息
        List<BuyCarrierTransportDTO> carrierTransportDTOS = buyCarrierTransportService
                .getToFactoryCarrierTransport(transportSectionDTO.getTransportSectionId());

        transportSectionDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(transportSectionDTO.getTransportMode()));
        transportSectionDTO.setCarrierTransportDTOS(carrierTransportDTOS);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, transportSectionDTO);
    }

    /**
     * 根据运输段id查询运输段信息(全部承运商)
     * @param transportSectionId
     * @return
     */
    @Override
    public ResponseResult<BuyTransportSectionDTO> getTransportSectionAllDetail(String transportSectionId) {
        BuyTransportSection transportSection = buyTransportSectionMapper.selectOne(
                new BuyTransportSection().setTransportSectionId(transportSectionId));

        //处理运输段信息
        BuyTransportSectionDTO transportSectionDTO = BeanUtils.copyProperties(transportSection, BuyTransportSectionDTO.class);

        //查询运输段承运商信息
        List<BuyCarrierTransportDTO> carrierTransportDTOS = buyCarrierTransportService
                .getCarrierTransportByBatch(transportSectionDTO.getTransportSectionId());

        transportSectionDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(transportSectionDTO.getTransportMode()));
        transportSectionDTO.setCarrierTransportDTOS(carrierTransportDTOS);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, transportSectionDTO);
    }

    /**
     * 保存运输段信息，返回运输段的运量
     * @param transportSectionDTO
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal saveTransportSectionInfo(BuyTransportSectionDTO transportSectionDTO, AuthPlatformUserInfo userInfo) {

        //运量
        BigDecimal freightVolume = BigDecimal.ZERO;
        BuyTransportSection transportSection;

        //判断运输段是否存在
        if (!StringUtils.isEmpty(transportSectionDTO.getTransportSectionId())) {
            transportSection = buyTransportSectionMapper.selectOne(new BuyTransportSection()
                    .setTransportSectionId(transportSectionDTO.getTransportSectionId()));
        } else {
            transportSection = BeanUtils.copyProperties(transportSectionDTO, BuyTransportSection.class);
            //运输段id
            String transportSectionId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.TRANSPORT_SECTION.getKey());
            transportSection.setTransportSectionId(transportSectionId);

            //地点查询
            if (transportSection.getStartingPlaceId() != null) {
                ConfigLocation configLocation = configLocationMapper.selectById(transportSection.getStartingPlaceId());
                if (!ObjectUtils.isEmpty(configLocation)) {
                    transportSection.setStartingPlace(configLocation.getName());
                }
            }
            if (transportSection.getArrivalPlaceId() != null) {
                ConfigLocation configLocation = configLocationMapper.selectById(transportSection.getArrivalPlaceId());
                if (!ObjectUtils.isEmpty(configLocation)) {
                    transportSection.setArrivalPlace(configLocation.getName());
                }
            }

            transportSection.setStatus(Status.TRUE.getKey());
            transportSection.setCreateId(userInfo.getId());
            transportSection.setCreator(userInfo.getName());
            transportSection.setCreateTime(new Date());
            if (buyTransportSectionMapper.insert(transportSection) <= 0) {
                log.error("保存批次运输段情况失败！");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_TRANSPORT_SECTION_FAILED);
            }
        }

        //保存承运商相关信息，并返回承运商总运量
        if (!CollectionUtils.isEmpty(transportSectionDTO.getCarrierTransportDTOS())) {
            for (BuyCarrierTransportDTO carrierTransportDTO : transportSectionDTO.getCarrierTransportDTOS()) {
                //运输段id
                carrierTransportDTO.setTransportSectionId(transportSection.getTransportSectionId());
                //合同业务id
                carrierTransportDTO.setContractBusinessId(transportSection.getContractBusinessId());
                //批次id
                carrierTransportDTO.setBatchId(transportSection.getBatchId());
                //运输模式
                carrierTransportDTO.setTransportMode(transportSection.getTransportMode());
                carrierTransportDTO.setToFactory(transportSection.getToFactory());
                carrierTransportDTO.setStartingPlaceId(transportSection.getStartingPlaceId());
                carrierTransportDTO.setStartingPlace(transportSection.getStartingPlace());
                carrierTransportDTO.setArrivalPlaceId(transportSection.getArrivalPlaceId());
                carrierTransportDTO.setArrivalPlace(transportSection.getArrivalPlace());
                freightVolume = freightVolume.add(buyCarrierTransportService.saveCarrierTransportInfo(carrierTransportDTO, userInfo));
            }
        }

        //返回第一段的运量
        if (transportSectionDTO.getTransportSection() == 0) {
            return freightVolume;
        } else {
            return BigDecimal.ZERO;
        }
    }

    /**
     * 单独保存运输段信息
     * @param transportSectionDTO
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveTransportSection(BuyTransportSectionDTO transportSectionDTO, AuthPlatformUserInfo userInfo) {

        if (StringUtils.isEmpty(transportSectionDTO.getContractBusinessId())
                || StringUtils.isEmpty(transportSectionDTO.getBatchId())) {
            log.error("合同业务id为空或者批次id为空，不能保存！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        //保存运输段信息
        BigDecimal freightVolume = saveTransportSectionInfo(transportSectionDTO, userInfo);

        //更新批次的运量和合同的在途量
        return updateContractFreightVolume(transportSectionDTO.getContractBusinessId(),
                transportSectionDTO.getBatchId(), freightVolume, userInfo);
    }

    /**
     * 新增物流批次运输段信息
     * @param transportSectionDTO
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveNewTransportSection(BuyTransportSectionDTO transportSectionDTO, AuthPlatformUserInfo userInfo) {

        if (StringUtils.isEmpty(transportSectionDTO.getContractBusinessId())
                || StringUtils.isEmpty(transportSectionDTO.getBatchId())) {
            log.error("合同业务id为空或者批次id为空，不能保存！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        BuyTransportSection transportSection = BeanUtils.copyProperties(transportSectionDTO, BuyTransportSection.class);
        //运输段id
        String transportSectionId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.TRANSPORT_SECTION.getKey());
        transportSection.setTransportSectionId(transportSectionId);

        //地点查询
        if (transportSection.getStartingPlaceId() != null) {
            ConfigLocation configLocation = configLocationMapper.selectById(transportSection.getStartingPlaceId());
            if (!ObjectUtils.isEmpty(configLocation)) {
                transportSection.setStartingPlace(configLocation.getName());
            }
        }
        if (transportSection.getArrivalPlaceId() != null) {
            ConfigLocation configLocation = configLocationMapper.selectById(transportSection.getArrivalPlaceId());
            if (!ObjectUtils.isEmpty(configLocation)) {
                transportSection.setArrivalPlace(configLocation.getName());
            }
        }

        transportSection.setStatus(Status.TRUE.getKey());
        transportSection.setCreateId(userInfo.getId());
        transportSection.setCreator(userInfo.getName());
        transportSection.setCreateTime(new Date());
        if (buyTransportSectionMapper.insert(transportSection) <= 0) {
            log.error("保存批次运输段情况失败！");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_TRANSPORT_SECTION_FAILED);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 更新批次的运量和合同的在途量
     * @param contractBusinessId
     * @param batchId
     * @param freightVolume
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateContractFreightVolume(String contractBusinessId, String batchId,
                                                                   BigDecimal freightVolume, AuthPlatformUserInfo userInfo) {

        //查询批次信息
        BuyLogisticsBatch logisticsBatch = buyLogisticsBatchMapper.selectOne(new BuyLogisticsBatch()
                .setBatchId(batchId));
        if (ObjectUtils.isEmpty(logisticsBatch) || ObjectUtils.isEmpty(logisticsBatch.getId())) {
            log.error("未查询到批次信息，保存失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //更新批次运量
        logisticsBatch.setFreightVolume(logisticsBatch.getFreightVolume().add(freightVolume));

        //更新操作
        logisticsBatch.setModifyId(userInfo.getId());
        logisticsBatch.setModifier(userInfo.getName());
        logisticsBatch.setModifyTime(new Date());
        if (buyLogisticsBatchMapper.updateById(logisticsBatch) <= 0) {
            log.error("更新批次信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //批次未确认的状态下，不更新合同的在途量
        if (Status.FALSE.getKey().equals(logisticsBatch.getStatus())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，保存失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //已发量
        BigDecimal issuedVolume = contractBasic.getIssuedVolume().add(freightVolume);
        //未发量
        BigDecimal undeliveredVolume = contractBasic.getQuantity().subtract(issuedVolume);
        //在途量
        BigDecimal trafficVolume = issuedVolume.subtract(contractBasic.getArrivalVolume());

        //更新合同各种数量
        contractBasic.setIssuedVolume(issuedVolume);
        contractBasic.setUndeliveredVolume(undeliveredVolume.compareTo(BigDecimal.ZERO) > 0 ? undeliveredVolume : BigDecimal.ZERO);
        contractBasic.setTrafficVolume(trafficVolume.compareTo(BigDecimal.ZERO) > 0 ? trafficVolume : BigDecimal.ZERO);
        contractBasic.setModifyId(userInfo.getId());
        contractBasic.setModifier(userInfo.getName());
        contractBasic.setModifyTime(new Date());
        if (buyContractBasicMapper.updateById(contractBasic) <= 0) {
            log.error("更新合同信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 更新合同的到货量
     * @param contractBusinessId
     * @param volume
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> updateContractArrivalVolume(String contractBusinessId, BigDecimal volume,
                                                                   AuthPlatformUserInfo userInfo) {

        if (ObjectUtils.isEmpty(contractBusinessId) || ObjectUtils.isEmpty(volume)) {
            log.error("未传合同业务id或者数量为空，保存合同到货量失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，保存合同到货量失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //已到量
        BigDecimal arrivalVolume = contractBasic.getArrivalVolume().add(volume);
        //在途量
        BigDecimal trafficVolume = contractBasic.getIssuedVolume().subtract(arrivalVolume);

        //更新合同各种数量
        contractBasic.setArrivalVolume(arrivalVolume);
        contractBasic.setTrafficVolume(trafficVolume.compareTo(BigDecimal.ZERO) > 0 ? trafficVolume : BigDecimal.ZERO);
        contractBasic.setModifyId(userInfo.getId());
        contractBasic.setModifier(userInfo.getName());
        contractBasic.setModifyTime(new Date());
        if (buyContractBasicMapper.updateById(contractBasic) <= 0) {
            log.error("更新合同信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 根据批次id单独查询运输段信息
     * @param batchId
     * @return
     */
    @Override
    public List<BuyTransportSectionDTO> getTransportSectionInfoByBatchId(String batchId) {

        List<BuyTransportSectionDTO> transportSectionDTOS = new ArrayList<>();

        //根据批次id查询运输段信息
        List<BuyTransportSection> transportSections = buyTransportSectionMapper.selectList(
                new EntityWrapper<BuyTransportSection>().eq("batch_id", batchId)
                        .eq("status", Status.TRUE.getKey()).orderBy("transport_section", true));
        if (CollectionUtils.isEmpty(transportSections)) {
            return transportSectionDTOS;
        }

        //遍历运输段信息，处理承运商相关信息
        for (BuyTransportSection transportSection : transportSections) {
            //处理运输段信息
            BuyTransportSectionDTO transportSectionDTO = BeanUtils.copyProperties(transportSection, BuyTransportSectionDTO.class);

            transportSectionDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(transportSectionDTO.getTransportMode()));
            transportSectionDTOS.add(transportSectionDTO);
        }

        return transportSectionDTOS;
    }

    /**
     * 保存运输段信息
     * @param transportSectionRq
     * @param logisticsBatch
     * @param userInfo
     * @return
     */
    @Override
    public void saveTransportSection(SaveTransportSectionRq transportSectionRq, BuyLogisticsBatch logisticsBatch, AuthPlatformUserInfo userInfo) {

        BuyTransportSection transportSection = BeanUtils.copyProperties(transportSectionRq, BuyTransportSection.class);
        //运输段id
        String transportSectionId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.TRANSPORT_SECTION.getKey());
        transportSection.setTransportSectionId(transportSectionId);

        transportSection.setContractBusinessId(logisticsBatch.getContractBusinessId());
        transportSection.setBatchId(logisticsBatch.getBatchId());

        //地点查询
        if (transportSection.getStartingPlaceId() != null) {
            ConfigLocation configLocation = configLocationMapper.selectById(transportSection.getStartingPlaceId());
            if (!ObjectUtils.isEmpty(configLocation)) {
                transportSection.setStartingPlace(configLocation.getName());
            }
        }
        if (transportSection.getArrivalPlaceId() != null) {
            ConfigLocation configLocation = configLocationMapper.selectById(transportSection.getArrivalPlaceId());
            if (!ObjectUtils.isEmpty(configLocation)) {
                transportSection.setArrivalPlace(configLocation.getName());
            }
        }

        transportSection.setStatus(Status.TRUE.getKey());
        transportSection.setCreateId(userInfo.getId());
        transportSection.setCreator(userInfo.getName());
        transportSection.setCreateTime(new Date());
        if (buyTransportSectionMapper.insert(transportSection) <= 0) {
            log.error("保存批次运输段情况失败！");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.SAVE_TRANSPORT_SECTION_FAILED);
        }
    }

    /**
     * 根据运输段id单独查询运输段详细信息
     * @param transportSectionId
     * @return
     */
    @Override
    public ResponseResult<BuyTransportSectionDTO> getTransportSectionById(String transportSectionId) {
        BuyTransportSection transportSection = buyTransportSectionMapper.selectOne(
                new BuyTransportSection().setTransportSectionId(transportSectionId).setStatus(Status.TRUE.getKey()));
        //处理运输段信息
        BuyTransportSectionDTO transportSectionDTO = BeanUtils.copyProperties(transportSection, BuyTransportSectionDTO.class);
        transportSectionDTO.setTransportModeName(EnumLogistics.transport_mode.getValue(transportSectionDTO.getTransportMode()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, transportSectionDTO);
    }

}
