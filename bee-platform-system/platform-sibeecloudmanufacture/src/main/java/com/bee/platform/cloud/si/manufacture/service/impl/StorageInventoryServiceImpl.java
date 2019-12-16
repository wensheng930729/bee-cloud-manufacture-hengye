package com.bee.platform.cloud.si.manufacture.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.StorageInventoryMapper;
import com.bee.platform.cloud.si.manufacture.dto.ProductStorageDTO;
import com.bee.platform.cloud.si.manufacture.entity.StorageInventory;
import com.bee.platform.cloud.si.manufacture.service.StorageInventoryService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-25
 */
@Service
public class StorageInventoryServiceImpl extends ServiceImpl<StorageInventoryMapper, StorageInventory> implements StorageInventoryService {

    @Autowired
    private StorageInventoryMapper storageInventoryMapper;

    /**
     * @descriptin 根据产品id查询仓库列表
     * @author xin.huang
     * @param productId
     * @date 2019/9/25
     * @return a
     */
    @Override
    public ResponseResult<List<ProductStorageDTO>> findStorages(Integer productId) {
        List<StorageInventory> storageInventories = this.selectList(new EntityWrapper<>(new StorageInventory()
                .setProductId(productId)
                .setStatus(Status.TRUE.getKey())));
        List<ProductStorageDTO> productStorageDTOS = new ArrayList<>(storageInventories.size());
        if (!CollectionUtils.isEmpty(storageInventories)) {
            storageInventories.forEach(storage -> {
                ProductStorageDTO dto = new ProductStorageDTO();
                dto.setProductId(productId)
                        .setStorageId(storage.getStorageId())
                        .setStorageName(storage.getStorageName());
                productStorageDTOS.add(dto);
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, productStorageDTOS);
    }

    /**
     * @descriptin 查询库存中已存在的产品列表
     * @author xin.huang
     * @param
     * @date 2019/10/10
     * @return
     */
    @Override
    public ResponseResult<List<ProductStorageDTO>> findStorageProducts(AuthPlatformUserInfo userInfo) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, storageInventoryMapper
                .findStorageProducts(userInfo.getOrgId(), userInfo.getFactoryId()));
    }

    @Override
    public boolean updateStorageInventories(List<StorageInventory> storageInventories) {
        return this.updateBatchById(storageInventories);
    }
    /**
     * @notes: 根据产品规格，查询各规格在仓库中的数量情况
     * @Author: junyang.li
     * @Date: 15:27 2019/11/26
     * @param productSpecIds : 产品规格id
     * @param factoryId : 当前用户工厂id
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.StorageInventory>
     */
    @Override
    public List<StorageInventory> getStorageByProductSpecIds(Integer factoryId,List<Integer> productSpecIds) {
        if(CollectionUtils.isEmpty(productSpecIds)){
            return new ArrayList<>();
        }
        return this.selectList(new EntityWrapper<StorageInventory>().where("status =1 and factory_id={0}",factoryId)
                .and()
                .in("product_spec_id",productSpecIds));
    }
    /**
     * @notes: 根据仓库id查询仓库中产品详细
     * @Author: junyang.li
     * @Date: 15:27 2019/11/26
     * @param storageIds : 仓库id
     * @param factoryId : 当前用户工厂id
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.StorageInventory>
     */
    @Override
    public List<StorageInventory> getStorageByStorageIds(Integer factoryId, List<Integer> storageIds) {
        if(factoryId == null){
            return new ArrayList<>();
        }
        Wrapper<StorageInventory> wrapper=new EntityWrapper<StorageInventory>()
                .where("status =1 and factory_id={0}",factoryId);
        if(!CollectionUtils.isEmpty(storageIds)){
            wrapper.and().in("storage_id",storageIds);
        }
        return this.selectList(wrapper);
    }


}
