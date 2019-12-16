package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigPlcDeviceMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigPlcDeviceDTO;
import com.bee.platform.cloud.si.manufacture.dto.PlcFieldConfigDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigPlcDevice;
import com.bee.platform.cloud.si.manufacture.entity.PlcFieldConfig;
import com.bee.platform.cloud.si.manufacture.rq.ConfigPlcDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigPlcDeviceUpdateRQ;
import com.bee.platform.cloud.si.manufacture.rq.PlcFieldConfigRQ;
import com.bee.platform.cloud.si.manufacture.rq.PlcInfoRQ;
import com.bee.platform.cloud.si.manufacture.service.PlcFieldConfigService;
import com.bee.platform.cloud.si.manufacture.rq.PlcFieldConfigRQ;
import com.bee.platform.cloud.si.manufacture.service.PlcFieldConfigService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigPlcDeviceService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * PLC设备档案 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigPlcDeviceServiceImpl extends ServiceImpl<ConfigPlcDeviceMapper, ConfigPlcDevice> implements ConfigPlcDeviceService {


    @Autowired
    private PlcFieldConfigService plcFieldConfigService;
    /**
     * 搜索PLC设备列表
     * @param userInfo 用户信息
     * @param deviceName 设备名称
     * @param status 状态
     * @param page 分页对象
     * @return PLC设备列表
     */
    @Override
    public ResponseResult<List<ConfigPlcDeviceDTO>> searchPlcDeviceList(AuthPlatformUserInfo userInfo, String deviceName, Integer status, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigPlcDevice> wrapper = new EntityWrapper<ConfigPlcDevice>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (StringUtils.isNotEmpty(deviceName)) {
            wrapper.like("name", deviceName);
        }
        if (!ObjectUtils.isEmpty(status)) {
            wrapper.eq("status", status);
        }
        List<ConfigPlcDevice> plcDeviceList = baseMapper.selectPage(pagination, wrapper);
        List<ConfigPlcDeviceDTO> dto = BeanUtils.assemble(ConfigPlcDeviceDTO.class, plcDeviceList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));


    }
    /**
     * 保存PLC设备信息
     * @param userInfo 用户信息
     * @param rq PLC设备信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult<ResCodeEnum> savePlcDevice(AuthPlatformUserInfo userInfo, ConfigPlcDeviceSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();

        ConfigPlcDevice configPlcDevice = BeanUtils.copyProperties(rq, ConfigPlcDevice.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);
        // 保存设备信息
        if (!insert(configPlcDevice)) {
            log.error("保存PLC设备信息失败，调用{}的{}方法出错", "ConfigPlcDeviceServiceImpl", "savePlcDevice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PLC_DEVICE_SAVE_FAILED);

        }
        //判断是否需要新增漏斗
        List<PlcFieldConfigRQ> list=rq.getFieldConfig();
        if(!CollectionUtils.isEmpty(list)){
            //查查询出PLCid
            return plcFieldConfigService.addPlcFields(userInfo,configPlcDevice.getId(),list);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * 修改PLC设备信息
     * @param userInfo 用户信息
     * @param rq PLC设备信息
     * @return ResCodeEnum
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult<ResCodeEnum> updatePlcDevice(AuthPlatformUserInfo userInfo, ConfigPlcDeviceUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer id = rq.getId();

        // 校验此id的PLC设备数据是否存在
        List<ConfigPlcDevice> have = selectList(new EntityWrapper<ConfigPlcDevice>()
                .eq("enterprise_id", enterpriseId)
                .eq("id",id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改PLC设备信息失败,此idPLC设备不存在,调用{}的{}方法出错", "ConfigPlcDeviceServiceImpl", "updatePlcDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PLC_DEVICE_UPDATE_FAILED_NO_DATA);
        }

        ConfigPlcDevice configPlcDevice = BeanUtils.copyProperties(rq, ConfigPlcDevice.class)
                .setModifyId(userId).setModifier(userName).setModifyTime(time);
        // 修改PLC设备信息
        if (!updateById(configPlcDevice)) {
            log.error("修改PLC设备信息失败，调用{}的{}方法出错", "ConfigPlcDeviceServiceImpl", "updatePlcDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PLC_DEVICE_UPDATE_FAILED);

        }
        //修改漏斗相关信息
        List<PlcFieldConfigRQ> list=rq.getFieldConfig();
        if(!CollectionUtils.isEmpty(list)){
            //查查询出PLCid
            return plcFieldConfigService.updatePlcFields(userInfo,configPlcDevice.getId(),list);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
    /**
     * 根据id删除PLC设备信息
     * @param userInfo 用户信息
     * @param id id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deletePlcDeviceById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        ConfigPlcDevice exist = selectOne(new EntityWrapper<ConfigPlcDevice>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在设备信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除PLC设备信息，没有找到相应数据，id为："+id);
            return;
        }
        // 删除设备信息
        if (!updateById(new ConfigPlcDevice()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除PLC设备信息失败，调用{}的{}方法出错", "ConfigPlcDeviceServiceImpl", "deletePlcDeviceById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.PLC_DEVICE_DELETED_FAILED);
        }

        // 删除旧的漏斗列表
        Wrapper<PlcFieldConfig> wrapper = new EntityWrapper<PlcFieldConfig>()
                .eq("factory_id", userInfo.getFactoryId())
                .eq("plc_id", id)
                .eq("deleted", Status.FALSE.getKey());
        List<PlcFieldConfig> plcFieldConfigs = plcFieldConfigService.selectList(wrapper);
        if(!CollectionUtils.isEmpty(plcFieldConfigs) && !plcFieldConfigService.update(new PlcFieldConfig().setDeleted(Status.TRUE.getKey()),wrapper)){
            log.error("修改PLC设备信息,删除漏斗列表失败，调用{}的{}方法出错", "ConfigPlcDeviceServiceImpl", "updatePlcDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PLC_DEVICE_UPDATE_FAILED);

        }
    }

    /**
     * 根据登陆用户企业id查询企业下的PLC设备列表
     * @param userInfo 用户信息
     * @return  PLC设备列表
     */
    @Override
    public List<ConfigPlcDeviceDTO> getPlcDeviceList(AuthPlatformUserInfo userInfo) {
        List<ConfigPlcDevice> plcDevices = selectList(new EntityWrapper<ConfigPlcDevice>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status", Status.TRUE.getKey())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId()));
        return BeanUtils.assemble(ConfigPlcDeviceDTO.class,plcDevices);
    }


    @Override
    public ConfigPlcDeviceDTO getPlcDeviceById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigPlcDevice plcDevice = selectOne(new EntityWrapper<ConfigPlcDevice>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        ConfigPlcDeviceDTO dto= BeanUtils.copyProperties(plcDevice, ConfigPlcDeviceDTO.class);
        //查询下料漏斗信息
        List<PlcFieldConfigDTO> list=plcFieldConfigService.getPlcFields(id);
        return dto.setFieldConfigs(list);
    }
    /**
     * @notes: 通过工厂id 和plc id 查询plc详细
     * @Author: junyang.li
     * @Date: 19:35 2019/10/11
     * @param factoryId :
     * @param plcId :
     * @return: com.bee.platform.cloud.si.manufacture.entity.ConfigPlcDevice
     */
    @Override
    public ConfigPlcDevice getPlcByFactoryId(int factoryId, int plcId) {
        return this.selectOne(new EntityWrapper<ConfigPlcDevice>()
                .eq("id", plcId)
                .eq("factory_id", factoryId)
                .eq("deleted", Status.FALSE.getKey()));
    }
}
