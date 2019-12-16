package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.entity.ConfigOpeningInventoryOrderDetail;
import com.baomidou.mybatisplus.service.IService;

/**
 * <p>
 * 期初库存明细表 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigOpeningInventoryOrderDetailService extends IService<ConfigOpeningInventoryOrderDetail> {

    /**
     * 保存期初库存详情
      * @param detail 详情信息
     * @return id
     */
    Integer saveOpeningInventoryOrderDetail(ConfigOpeningInventoryOrderDetail detail);

}
