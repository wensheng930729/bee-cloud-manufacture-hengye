package com.bee.platform.cloud.si.manufacture.service.factoryconfig;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.ConfigPlcDeviceDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigPlcDevice;
import com.bee.platform.cloud.si.manufacture.rq.ConfigPlcDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigPlcDeviceUpdateRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * PLC设备档案 服务类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigPlcDeviceService extends IService<ConfigPlcDevice> {

    /**
     * 搜索PLC设备列表
     * @param userInfo 用户信息
     * @param deviceName 设备名称
     * @param status 状态
     * @param page 分页对象
     * @return PLC设备列表
     */
    ResponseResult<List<ConfigPlcDeviceDTO>> searchPlcDeviceList(AuthPlatformUserInfo userInfo, String deviceName, Integer status, Page page);

    /**
     * 保存PLC设备信息
     * @param userInfo 用户信息
     * @param rq PLC设备信息
     * @return id
     */
    ResponseResult<ResCodeEnum> savePlcDevice(AuthPlatformUserInfo userInfo, ConfigPlcDeviceSaveRQ rq);

    /**
     * 修改PLC设备信息
     * @param userInfo 用户信息
     * @param rq PLC设备信息
     * @return ResCodeEnum
     */
    ResponseResult<ResCodeEnum> updatePlcDevice(AuthPlatformUserInfo userInfo, ConfigPlcDeviceUpdateRQ rq);

    /**
     * 根据id删除PLC设备信息
     * @param userInfo 用户信息
     * @param id id
     */
    void deletePlcDeviceById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 根据登陆用户企业id查询企业下的PLC设备列表
     * @param userInfo 用户信息
     * @return  PLC设备列表
     */
    List<ConfigPlcDeviceDTO> getPlcDeviceList(AuthPlatformUserInfo userInfo);

    ConfigPlcDeviceDTO getPlcDeviceById(AuthPlatformUserInfo userInfo, Integer id);
    /**
     * @notes: 通过工厂id 和plc id 查询plc详细
     * @Author: junyang.li
     * @Date: 19:35 2019/10/11
     * @param factoryId :
     * @param plcId :
     * @return: com.bee.platform.cloud.si.manufacture.entity.ConfigPlcDevice
     */
    ConfigPlcDevice getPlcByFactoryId(int factoryId,int plcId);
}
