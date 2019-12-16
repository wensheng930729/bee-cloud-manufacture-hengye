package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigDeviceInspectionDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigDeviceInspection;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceInspectionSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceInspectionUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 设备巡检 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigDeviceInspectionService extends IService<ConfigDeviceInspection> {
    /**
     * 搜索设备巡检信息列表
     * @param userInfo 用户信息
     * @param deviceName 设备名称
     * @param page 分页对象
     * @return 设备巡检列表
     */
    ResponseResult<List<ConfigDeviceInspectionDTO>> searchDeviceInspectionList(AuthPlatformUserInfo userInfo, String deviceName, Page page);

    /**
     * 保存设备巡检信息
     * @param userInfo 用户信息
     * @param rq 设备巡检信息
     * @return 设备巡检id
     */
    Integer saveDeviceInspection(AuthPlatformUserInfo userInfo, ConfigDeviceInspectionSaveRQ rq);

    /**
     * 修改设备巡检信息
     * @param userInfo  用户信息
     * @param rq 设备巡检信息
     * @return 设备巡检id
     */
    Integer updateDeviceInspection(AuthPlatformUserInfo userInfo, ConfigDeviceInspectionUpdateRQ rq);

    /**
     * 根据id删除设备巡检信息
     * @param userInfo 用户信息
     * @param id 设备巡检id
     */
    void deleteDeviceInspectionById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 根据设备巡检编号查询是否存在设备巡检信息
     * @param userInfo 用户信息
     * @param deviceCode 设备编号
     * @return 结果 true 有数据 false 无数据
     */
    Boolean checkDeviceInspection(AuthPlatformUserInfo userInfo, String deviceCode);

    /**
     * 根据设备编号查询设备巡检信息
     * @param userInfo 用户信息
     * @param deviceCode 设备编号
     * @return 设备巡检信息
     */
    ConfigDeviceInspectionDTO getDeviceInspectionByCode(AuthPlatformUserInfo userInfo, String deviceCode);

    ConfigDeviceInspectionDTO getDeviceInspectionById(AuthPlatformUserInfo userInfo, Integer id);
}
