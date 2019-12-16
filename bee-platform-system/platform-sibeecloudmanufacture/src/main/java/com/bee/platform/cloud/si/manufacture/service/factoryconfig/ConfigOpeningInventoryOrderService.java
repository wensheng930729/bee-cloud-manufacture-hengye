package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigOpeningInventoryOrderDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigOpeningInventorySearchDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigOpeningInventoryOrder;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigOpeningInventoryOrderRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigOpeningInventorySearchRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 期初库存主表 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigOpeningInventoryOrderService extends IService<ConfigOpeningInventoryOrder> {
    /**
     * 条件查询期初库存
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @param page 分页对象
     * @return 期初库存信息
     */
    ResponseResult<List<ConfigOpeningInventorySearchDTO>> searchOpeningInventoryByCondition(AuthPlatformUserInfo userInfo, ConfigOpeningInventorySearchRQ rq, Page page);

    /**
     * 根据id查看期初库存详情
     * @param id id
     * @return 期初库存详情
     */
    ConfigOpeningInventoryOrderDTO getOpeningInventoryById(Integer id);

    /**
     * 保存期初库存详情
     * @param userInfo 用户详情
     * @param rq 期初库存信息
     * @return id
     */
    Integer saveOpeningInventoryOrder(AuthPlatformUserInfo userInfo, ConfigOpeningInventoryOrderRQ rq);

    /**
     * 生成编号
     * @return 编号
     */
    String generateCode();
}
