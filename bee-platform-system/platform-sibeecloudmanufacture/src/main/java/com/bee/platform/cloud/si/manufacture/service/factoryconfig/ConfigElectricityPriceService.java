package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigAmmeterDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigElectricityPriceDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigElectricityPrice;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigElectricityPriceService extends IService<ConfigElectricityPrice> {
    /**
     * 搜索电价管理列表
     * @param userInfo 用户信息
     * @param page 分页对象
     * @return 电价列表
     */
    ResponseResult<List<ConfigElectricityPriceDTO>> searchElectricityPriceList(AuthPlatformUserInfo userInfo, Page page);

    /**
     * 保存电价信息
     * @param userInfo 用户信息
     * @param rq 电价信息
     * @return 电价id
     */
    Integer saveElectricityPrice(AuthPlatformUserInfo userInfo, ConfigElectricityPriceSaveRQ rq);

    /**
     * 修改电价信息
     * @param userInfo 用户信息
     * @param rq 电价信息
     * @return 电价id
     */
    Integer updateElectricityPrice(AuthPlatformUserInfo userInfo, ConfigElectricityPriceUpdateRQ rq);

    /**
     * 根据id删除电价信息
     * @param userInfo 用户信息
     * @param id 电价id
     */
    void deleteElectricityPriceById(AuthPlatformUserInfo userInfo, Integer id);

    ConfigElectricityPriceDTO getElectricityPriceById(AuthPlatformUserInfo userInfo, Integer id);
}
