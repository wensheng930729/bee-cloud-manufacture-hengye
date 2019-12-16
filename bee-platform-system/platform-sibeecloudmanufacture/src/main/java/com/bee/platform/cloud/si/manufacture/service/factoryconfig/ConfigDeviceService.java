package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.bee.platform.cloud.si.manufacture.dto.ConfigDeviceDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigDevice;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 设备档案 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigDeviceService extends IService<ConfigDevice> {
    /**
     * 搜索设备列表
     * @param userInfo 用户信息
     * @param deviceName 设备名称
     * @param status 设备状态
     * @param page 分页对象
     * @return 设备列表
     */
    ResponseResult<List<ConfigDeviceDTO>> searchDeviceList(AuthPlatformUserInfo userInfo, String deviceName, Integer status, Page page);

    /**
     * 保存设备信息
     * @param userInfo 用户信息
     * @param rq 设备信息
     * @return 设备id
     */
    Integer saveDevice(AuthPlatformUserInfo userInfo, ConfigDeviceSaveRQ rq);

    /**
     * 修改设备信息
     * @param userInfo 用户信息
     * @param rq  设备信息
     * @return 设备id
     */
    Integer updateDevice(AuthPlatformUserInfo userInfo, ConfigDeviceUpdateRQ rq);

    /**
     * 根据id删除设备信息
     * @param userInfo 用户信息
     * @param id 设备id
     */
    void deleteDeviceById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 根据类型查询设备列表
     * @param userInfo 用户信息
     * @param types 设备类型
     * @return 设备列表
     */
    List<ConfigDeviceDTO> getDeviceListByType(AuthPlatformUserInfo userInfo, List<Integer> types);

    /**
     * 根据设备名称模糊搜索设备下拉列表
     * @param userInfo 用户信息
     * @param deviceName 设备名称
     * @return 设备列表
     */
    List<ConfigDeviceDTO> getDeviceListByName(AuthPlatformUserInfo userInfo, String deviceName);

    ConfigDeviceDTO getDeviceById(AuthPlatformUserInfo userInfo, Integer id);
}
