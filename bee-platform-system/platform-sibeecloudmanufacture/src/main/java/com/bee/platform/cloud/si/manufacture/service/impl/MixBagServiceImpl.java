package com.bee.platform.cloud.si.manufacture.service.impl;


import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigProductSpecMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.FinishedProductPendingStorageMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProBaggingMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.StorageInventoryMapper;
import com.bee.platform.cloud.si.manufacture.dto.DetailBySampleCodeProDTO;
import com.bee.platform.cloud.si.manufacture.dto.TonBagDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumStorage;
import com.bee.platform.cloud.si.manufacture.rq.BaggingUpdateRq;
import com.bee.platform.cloud.si.manufacture.service.MixBagService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRepositoryService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 * @ClassName: MixBagServiceImpl
 * @Description: 混袋相关Service
 * @Author: fei.sun
 * @Date: 2019/11/25 15:19
 * @Version: 1.0
 */
@Service
@Slf4j
public class MixBagServiceImpl implements MixBagService {

    @Autowired
    private FinishedProductPendingStorageMapper finishedProductPendingStorageMapper;

    @Autowired
    private StorageInventoryMapper storageInventoryMapper;

    @Autowired
    private SampleServiceImpl sampleService;

    @Autowired
    private ConfigRepositoryService repositoryService;

    @Autowired
    private ProBaggingMapper proBaggingMapper;

    @Autowired
    private ConfigProductSpecMapper configProductSpecMapper;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void confirmMixBag(String majorTonBagNum, String tonBagNum, BigDecimal mixBagAmount, AuthPlatformUserInfo userInfo) {
        //
        FinishedProductPendingStorage majorTonBag = finishedProductPendingStorageMapper.selectOne(
                new FinishedProductPendingStorage().setTonBagNumber(majorTonBagNum)
                        .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                        .setStatus(EnumStorage.Status.normal.getKey()));
        FinishedProductPendingStorage tonBag = finishedProductPendingStorageMapper.selectOne(
                new FinishedProductPendingStorage().setTonBagNumber(tonBagNum)
                        .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                        .setStatus(EnumStorage.Status.normal.getKey()));
        if (ObjectUtils.isEmpty(majorTonBag)||ObjectUtils.isEmpty(tonBag)) {
            throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND,"该吨袋编码没有找到相应的数据！");
        }
        if (tonBag.getProductNumber().compareTo(mixBagAmount)< 0) {
            throw new BusinessException(ResCodeEnum.CONFIRM_FAILED,"要混袋的数量大于吨袋实际数量！");
        }
        //不是同一种产品不能进行混袋
        if (!majorTonBag.getProductId().equals(tonBag.getProductId())) {
            throw new BusinessException(ResCodeEnum.CONFIRM_FAILED,"不是同一种产品不能进行混袋！");
        }
        //如果是入库的产品
        if (!ObjectUtils.isEmpty(majorTonBag.getProductSpecId())&&EnumStorage.PutStorage.storage.getKey().equals(majorTonBag.getPutStorage())) {
            //增加库存
            updateStock(majorTonBag.getStorageId(),majorTonBag.getProductId(),majorTonBag.getProductSpecId(),mixBagAmount);
        }
        //更新主吨袋的数量
        majorTonBag.setProductNumber(majorTonBag.getProductNumber().add(mixBagAmount)).updateById();
        //混袋已入库
        if (!ObjectUtils.isEmpty(tonBag.getProductSpecId())&&EnumStorage.PutStorage.storage.getKey().equals(tonBag.getPutStorage())) {
            //减少库存
            updateStock(tonBag.getStorageId(),tonBag.getProductId(),tonBag.getProductSpecId(),mixBagAmount.multiply(new BigDecimal("-1")));
        }
        //更新混袋后数量
        tonBag.setProductNumber(tonBag.getProductNumber().subtract(mixBagAmount)).updateById();
    }

    /**
     * 更新库存
     * @param storageId 仓库id
     * @param productId 产品id
     * @param productSpecId 规格id
     * @param productNum 更细数量
     */
    private void updateStock(Integer storageId,Integer productId,Integer productSpecId,BigDecimal productNum){
        StorageInventory storageInventory = storageInventoryMapper.selectOne(new StorageInventory()
                .setProductId(productId).setStorageId(storageId).setProductSpecId(productSpecId)
                .setStatus(EnumStorage.Status.normal.getKey()));
        if(ObjectUtils.isEmpty(storageInventory)){
            throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.PRODUCT_STORAGE_NOT_EXIST);
        }
       storageInventory.setProductNumber(storageInventory.getProductNumber().add(productNum)).updateById();
    }

    /**
     * 根据吨袋编号查询吨袋相关信息
     * @param tonBagNumber 吨袋编号
     * @return
     */
    @Override
    public ResponseResult<TonBagDetailDTO> getTonBagDetail(String tonBagNumber) {

        //查询入库吨袋信息
        List<FinishedProductPendingStorage> productPendingStorageList = finishedProductPendingStorageMapper
                .selectList(new EntityWrapper<FinishedProductPendingStorage>()
                        .eq("ton_bag_number", tonBagNumber).eq("status", Status.TRUE.getKey()));
        if (CollectionUtils.isEmpty(productPendingStorageList) || productPendingStorageList.size() > 1) {
            log.error("通过吨袋编号查询不到吨袋信息或者查出多条信息，请确认！");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        FinishedProductPendingStorage productPendingStorage = productPendingStorageList.get(0);

        TonBagDetailDTO tonBagDetailDTO = BeanUtils.copyProperties(productPendingStorage, TonBagDetailDTO.class);

        //判断是否可以编辑
        if (!ObjectUtils.isEmpty(productPendingStorage.getStorageId())
                && !ObjectUtils.isEmpty(productPendingStorage.getProductSpecId())
                && EnumStorage.PutStorage.storage.getKey().equals(productPendingStorage.getPutStorage())) {
            //仓库id不为空，规格不为空，已入库，才能进行编辑
            tonBagDetailDTO.setEditable(true);
        } else {
            tonBagDetailDTO.setEditable(false);
        }

        //查询仓库名称
        if (!ObjectUtils.isEmpty(productPendingStorage.getStorageId())) {
            ConfigRepository repository = repositoryService.selectById(productPendingStorage.getStorageId());
            if (!ObjectUtils.isEmpty(repository)) {
                tonBagDetailDTO.setStorageName(repository.getName());
            }
        }

        //查询吨袋样品编号
        ProBagging bagging = proBaggingMapper.selectOne(new ProBagging().setBaggingCode(tonBagNumber));
        if (ObjectUtils.isEmpty(bagging) || StringUtils.isEmpty(bagging.getSampleCode())) {
            log.error("通过吨袋编号查询不到成品装袋信息或者查询不出样品编号，请确认！");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }

        //查询吨袋相关质检信息
        ResponseResult result = sampleService.getDetailByCode(bagging.getSampleCode());
        if (ResCodeEnum.SUCCESS.code.equals(result.getCode()) && result.getObject() != null) {
            DetailBySampleCodeProDTO sampleCodeProDTO = (DetailBySampleCodeProDTO)result.getObject();
            tonBagDetailDTO.setSampleInfo(sampleCodeProDTO);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, tonBagDetailDTO);
    }

    /**
     * 更新吨袋相关信息
     * @param rq 吨袋信息修改入参
     * @param userInfo 用户信息
     * @return
     */
    @Override
    public ResponseResult<String> updateTonBagInfo(BaggingUpdateRq rq, AuthPlatformUserInfo userInfo) {

        //查询入库吨袋信息
        List<FinishedProductPendingStorage> productPendingStorageList = finishedProductPendingStorageMapper
                .selectList(new EntityWrapper<FinishedProductPendingStorage>()
                        .eq("ton_bag_number", rq.getBaggingCode()).eq("status", Status.TRUE.getKey()));
        if (CollectionUtils.isEmpty(productPendingStorageList) || productPendingStorageList.size() > 1) {
            log.error("通过吨袋编号查询不到吨袋信息或者查出多条信息，请确认！");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        FinishedProductPendingStorage productPendingStorage = productPendingStorageList.get(0);

        //比较吨袋信息是否进行了变更
        boolean equalProductNumber = rq.getProductNumber().compareTo(productPendingStorage.getProductNumber()) == 0;
        boolean equalProductSpecId = rq.getProductSpecId().equals(productPendingStorage.getProductSpecId());
        boolean equalStorageId = rq.getStorageId().equals(productPendingStorage.getStorageId());
        if (equalProductNumber && equalProductSpecId && equalStorageId) {
            log.info("吨袋信息没有更新。");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }

        //信息更新了
        //先进行出库操作
        updateStock(productPendingStorage.getStorageId(), productPendingStorage.getProductId(),
                productPendingStorage.getProductSpecId(), BigDecimal.ZERO.subtract(productPendingStorage.getProductNumber()));

        //查询库存名称
        ConfigProductSpec configProductSpec = configProductSpecMapper.selectById(rq.getProductSpecId());
        productPendingStorage.setStorageId(rq.getStorageId());
        productPendingStorage.setProductSpecId(rq.getProductSpecId());
        productPendingStorage.setProductSpecName(configProductSpec.getSpecName());
        productPendingStorage.setProductNumber(rq.getProductNumber());

        //再进行入库操作
        increaseInventory(productPendingStorage, userInfo);

        //最后更新吨袋信息
        productPendingStorage.setModifier(userInfo.getName());
        productPendingStorage.setModifyId(userInfo.getId());
        productPendingStorage.setModifyTime(LocalDateTime.now());
        if (finishedProductPendingStorageMapper.updateById(productPendingStorage) < 1) {
            log.error("更新吨袋相关信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 增加库存
     * @param productPendingStorage
     */
    private void increaseInventory(FinishedProductPendingStorage productPendingStorage, AuthPlatformUserInfo userInfo){

        StorageInventory oldStorageInventory = storageInventoryMapper.selectOne(new StorageInventory()
                .setProductId(productPendingStorage.getProductId()).setStorageId(productPendingStorage.getStorageId())
                .setProductSpecId(productPendingStorage.getProductSpecId()).setStatus(EnumStorage.Status.normal.getKey()));
        if(ObjectUtils.isEmpty(oldStorageInventory)){
            //新增库存
            StorageInventory storageInventory = new StorageInventory();
            storageInventory.setOrgId(userInfo.getOrgId());
            storageInventory.setFactoryId(userInfo.getFactoryId());
            storageInventory.setProductId(productPendingStorage.getProductId());
            storageInventory.setProductName(productPendingStorage.getProductName());
            storageInventory.setProductSpecId(productPendingStorage.getProductSpecId());
            storageInventory.setProductSpecName(productPendingStorage.getProductSpecName());
            storageInventory.setProductUnit(productPendingStorage.getProductUnit());
            storageInventory.setProductNumber(productPendingStorage.getProductNumber());
            storageInventory.setStorageId(productPendingStorage.getStorageId());
            //仓库名称
            ConfigRepository configRepository = repositoryService.selectById(productPendingStorage.getStorageId());
            storageInventory.setStorageName(configRepository.getName());
            storageInventory.setStatus(Status.TRUE.getKey());
            storageInventory.setCreateId(userInfo.getId());
            storageInventory.setCreator(userInfo.getName());
            storageInventory.setCreateTime(LocalDateTime.now());

            storageInventory.insert();
        }else {
            //更新库存数量
            BigDecimal productNumber = productPendingStorage.getProductNumber();
            oldStorageInventory.setProductNumber(oldStorageInventory.getProductNumber().add(productNumber));
            oldStorageInventory.setModifier(userInfo.getName())
                    .setModifyId(userInfo.getId()).setModifyTime(LocalDateTime.now());
            oldStorageInventory.updateById();
        }
    }

}
