package com.bee.platform.cloud.si.manufacture.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ProductStorageDTO;
import com.bee.platform.cloud.si.manufacture.entity.StorageInventory;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-25
 */
public interface StorageInventoryService extends IService<StorageInventory> {

    ResponseResult<List<ProductStorageDTO>> findStorages(Integer productId);

    ResponseResult<List<ProductStorageDTO>> findStorageProducts(AuthPlatformUserInfo userInfo);

    /**
     *  批量更新库存
     * @param storageInventories 待更新集合
     * @return 更新是否成功
     */
    boolean updateStorageInventories(List<StorageInventory> storageInventories);
    /**
     * @notes: 根据产品规格，查询各规格在仓库中的数量情况
     * @Author: junyang.li
     * @Date: 15:27 2019/11/26
     * @param productSpecIds : 产品规格id
     * @param factoryId : 当前用户工厂id
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.StorageInventory>
     */
    List<StorageInventory> getStorageByProductSpecIds(Integer factoryId,List<Integer> productSpecIds);

    /**
     * @notes: 根据仓库id查询仓库中产品详细
     * @Author: junyang.li
     * @Date: 15:27 2019/11/26
     * @param storageIds : 仓库id
     * @param factoryId : 当前用户工厂id
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.StorageInventory>
     */
    List<StorageInventory> getStorageByStorageIds(Integer factoryId,List<Integer> storageIds);
}
