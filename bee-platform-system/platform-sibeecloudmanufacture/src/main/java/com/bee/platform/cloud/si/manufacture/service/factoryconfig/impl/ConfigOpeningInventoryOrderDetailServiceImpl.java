package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigOpeningInventoryOrderDetailMapper;
import com.bee.platform.cloud.si.manufacture.entity.ConfigOpeningInventoryOrderDetail;
import com.bee.platform.cloud.si.manufacture.entity.StorageInventory;
import com.bee.platform.cloud.si.manufacture.service.StorageInventoryService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigOpeningInventoryOrderDetailService;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * <p>
 * 期初库存明细表 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigOpeningInventoryOrderDetailServiceImpl extends ServiceImpl<ConfigOpeningInventoryOrderDetailMapper, ConfigOpeningInventoryOrderDetail> implements ConfigOpeningInventoryOrderDetailService {


    @Autowired
    private StorageInventoryService storageInventoryService;

    /**
     * 保存期初库存详情
     *
     * @param detail 详情信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveOpeningInventoryOrderDetail(ConfigOpeningInventoryOrderDetail detail) {

        // 保存详情信息
        if (!insert(detail)) {
            log.error("保存期初库存明细失败，调用{}的{}方法出错", "ConfigOpeningInventoryOrderDetailServiceImpl", "saveOpeningInventoryOrderDetail()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.OPENING_INVENTORY_DETAIL_SAVE_FAILED);

        }
        // 修改库存信息
        Integer productId = detail.getProductId();
        String productName = detail.getProductName();
        Integer productSpecId = detail.getProductSpecId();
        String productSpecName = detail.getProductSpecName();
        Integer userId = detail.getCreateId();
        String userName = detail.getCreator();
        String unit = detail.getUnit();
        Integer repositoryId = detail.getRepositoryId();
        String repositoryName = detail.getRepositoryName();
        BigDecimal quantity = detail.getQuantity();
        Integer enterpriseId = detail.getEnterpriseId();
        Integer factoryId = detail.getFactoryId();
        // 查询库存是否有对应 产品id +仓库id +规格id 的记录
        StorageInventory storageInventory = storageInventoryService.selectOne(new EntityWrapper<StorageInventory>()
                .eq("org_id",enterpriseId)
                .eq("factory_id",factoryId)
                .eq("product_id", productId)
                .eq("product_spec_id", productSpecId)
                .eq("storage_id", repositoryId)
                .eq("status", Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(storageInventory)) {
            StorageInventory newStorage = new StorageInventory()
                    .setOrgId(enterpriseId)
                    .setFactoryId(factoryId)
                    .setProductId(productId)
                    .setProductName(productName)
                    .setProductSpecId(productSpecId)
                    .setProductSpecName(productSpecName)
                    .setProductNumber(quantity)
                    .setProductUnit(unit)
                    .setStorageId(repositoryId)
                    .setStorageName(repositoryName)
                    .setStatus(Status.TRUE.getKey())
                    .setCreateId(userId)
                    .setCreator(userName)
                    .setCreateTime(LocalDateTime.now());
            if (!storageInventoryService.insert(newStorage)) {
                log.error("保存期初库存明细，保存库存数量到库存表失败，调用{}的{}方法出错", "ConfigOpeningInventoryOrderDetailServiceImpl", "saveOpeningInventoryOrderDetail()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.OPENING_INVENTORY_DETAIL_SAVE_FAILED);
            }
        } else {
            // 库存有数据 将数量添加到库存数量上
            BigDecimal newNum = storageInventory.getProductNumber().add(quantity);
            storageInventory.setProductNumber(newNum)
                    .setModifyId(userId)
                    .setModifier(userName)
                    .setModifyTime(LocalDateTime.now());
            if (!storageInventoryService.updateById(storageInventory)) {
                log.error("保存期初库存明细，保存库存数量到库存表失败，调用{}的{}方法出错", "ConfigOpeningInventoryOrderDetailServiceImpl", "saveOpeningInventoryOrderDetail()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.OPENING_INVENTORY_DETAIL_SAVE_FAILED);

            }

        }


        return detail.getId();
    }
}
