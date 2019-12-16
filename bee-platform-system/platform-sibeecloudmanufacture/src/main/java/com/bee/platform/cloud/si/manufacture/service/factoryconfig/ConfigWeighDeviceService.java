package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ConfigWeighDeviceDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigWeighDevice;
import com.bee.platform.cloud.si.manufacture.rq.ConfigWeighDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigWeighDeviceUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 称重设备档案 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigWeighDeviceService extends IService<ConfigWeighDevice> {
    /**
     * 搜索称重设备列表
     *
     * @param userInfo        用户信息
     * @param weighDeviceName 称重设备名称
     * @param status          状态
     * @param page            分页对象
     * @return 称重设备列表
     */
    ResponseResult<List<ConfigWeighDeviceDTO>> searchWeighDeviceList(AuthPlatformUserInfo userInfo, String weighDeviceName, Integer status, Page page);

    /**
     * 保存称重设备
     *
     * @param userInfo 用户信息
     * @param rq       称重设备信息
     * @return id
     */
    Integer saveWeighDevice(AuthPlatformUserInfo userInfo, ConfigWeighDeviceSaveRQ rq);

    /**
     * 修改称重设备
     *
     * @param userInfo 用户信息
     * @param rq       称重设备信息
     * @return id
     */
    Integer updateWeighDevice(AuthPlatformUserInfo userInfo, ConfigWeighDeviceUpdateRQ rq);

    /**
     * 根据id删除称重设备
     *
     * @param userInfo 用户信息
     * @param id       称重设备id
     */
    void deleteWeighDeviceById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * @Description 获取称重设备信息
     * @author chenxm66777123
     * @Date 2019/9/27 9:54
     * @version 1.0.0
     */
    List<ConfigWeighDeviceDTO> getWeightSelectList(AuthPlatformUserInfo userInfo,Integer type);

    ConfigWeighDeviceDTO getWeighDeviceById(AuthPlatformUserInfo userInfo, Integer id);

    List<ConfigWeighDeviceDTO> getWeighDeviceListByType(AuthPlatformUserInfo userInfo, List<Integer> types);
}
