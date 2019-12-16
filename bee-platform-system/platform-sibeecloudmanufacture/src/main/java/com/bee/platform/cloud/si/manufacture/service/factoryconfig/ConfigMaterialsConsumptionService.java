package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigMaterialsConsumptionDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigMaterialsConsumption;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigMaterialsConsumptionSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigMaterialsConsumptionUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 原料吨耗 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigMaterialsConsumptionService extends IService<ConfigMaterialsConsumption> {

    /**
     * 条件搜索原料吨耗列表
     * @param userInfo 用户信息
     * @param productName 产品名称
     * @param page 分页对象
     * @return 原料吨耗信息
     */
    ResponseResult<List<ConfigMaterialsConsumptionDTO>> searchMaterialsConsumptionList(AuthPlatformUserInfo userInfo, String productName, Page page);

    /**
     * 保存原料吨耗信息
     * @param userInfo 用户信息
     * @param rq 原料吨耗信息
     * @return id
     */
    Integer saveMaterialsConsumption(AuthPlatformUserInfo userInfo, ConfigMaterialsConsumptionSaveRQ rq);

    /**
     * 修改原料吨耗信息
     * @param userInfo 用户信息
     * @param rq 原料吨耗信息
     * @return id
     */
    Integer updateMaterialsConsumption(AuthPlatformUserInfo userInfo, ConfigMaterialsConsumptionUpdateRQ rq);

    /**
     * 根据id删除原料吨耗信息
     * @param userInfo 用户信息
     * @param id id
     */
    void deleteMaterialsConsumptionById(AuthPlatformUserInfo userInfo, Integer id);

    ConfigMaterialsConsumptionDTO getMaterialsConsumptionById(AuthPlatformUserInfo userInfo, Integer id);
}
