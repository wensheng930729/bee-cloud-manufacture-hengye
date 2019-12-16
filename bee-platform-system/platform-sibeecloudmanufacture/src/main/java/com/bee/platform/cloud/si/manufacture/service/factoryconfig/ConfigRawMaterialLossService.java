package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ConfigRawMaterialLossDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigRawMaterialLoss;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRawMaterialLossSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRawMaterialLossUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 原料损耗配置表 服务类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
public interface ConfigRawMaterialLossService extends IService<ConfigRawMaterialLoss> {
    /**
     * 条件查询原料损耗列表
     * @param userInfo 用户信息
     * @param productName 产品名称
     * @param page 分页对象
     * @return 原料损耗列表
     */
    ResponseResult<List<ConfigRawMaterialLossDTO>> searchRawMaterialLossList(AuthPlatformUserInfo userInfo, String productName, Page page);

    /**
     * 保存原料损耗信息
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer saveRawMaterialLoss(AuthPlatformUserInfo userInfo, ConfigRawMaterialLossSaveRQ rq);

    /**
     * 修改原料损耗信息
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer updateRawMaterialLoss(AuthPlatformUserInfo userInfo, ConfigRawMaterialLossUpdateRQ rq);

    /**
     * 根据id删除原料损耗信息
     * @param userInfo 用户信息
     * @param id id
     */
    void deleteRawMaterialLossById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 根据id查询原料损耗信息
     * @param userInfo 用户信息
     * @param id id
     * @return 原料损耗信息
     */
    ConfigRawMaterialLossDTO getRawMaterialLossById(AuthPlatformUserInfo userInfo, Integer id);
}
