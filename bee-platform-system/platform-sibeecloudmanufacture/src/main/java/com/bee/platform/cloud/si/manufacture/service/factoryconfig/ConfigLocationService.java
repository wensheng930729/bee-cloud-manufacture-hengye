package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigLocationDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigLocation;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigLocationSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigLocationUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 物流地点管理表 服务类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
public interface ConfigLocationService extends IService<ConfigLocation> {
    /**
     * 条件查询物流地点列表
     * @param userInfo 用户信息
     * @param name 地点名称
     * @param page 分页对象
     * @return 地点列表
     */
    ResponseResult<List<ConfigLocationDTO>> searchLocationList(AuthPlatformUserInfo userInfo, String name, Page page);

    /**
     *
     * 保存物流地点信息
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer saveLocation(AuthPlatformUserInfo userInfo, ConfigLocationSaveRQ rq);

    /**
     * 修改物流地点
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer updateLocation(AuthPlatformUserInfo userInfo, ConfigLocationUpdateRQ rq);

    /**
     * 根据id删除物流地点
     * @param userInfo 用户信息
     * @param id id
     */
    void deleteLocationById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 根据id查询地点详情
     * @param userInfo 用户信息
     * @param id id
     * @return 地点详情
     */
    ConfigLocationDTO getLocationById(AuthPlatformUserInfo userInfo, Integer id);

    List<ConfigLocationDTO> getLocationList(AuthPlatformUserInfo userInfo);
}
