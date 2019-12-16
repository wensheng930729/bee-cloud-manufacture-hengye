package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigDeviceMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigDeviceDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigDevice;
import com.bee.platform.cloud.si.manufacture.entity.ProBlanking;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigDeviceService;
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
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProBlankingService;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 设备档案 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigDeviceServiceImpl extends ServiceImpl<ConfigDeviceMapper, ConfigDevice> implements ConfigDeviceService {

    @Autowired
    private ProBlankingService proBlankingService;

   // pro_ore_furnace_record


    /**
     * 搜索设备列表
     * @param userInfo 用户信息
     * @param deviceName 设备名称
     * @param status 设备状态
     * @param page 分页对象
     * @return 设备列表
     */
    @Override
    public ResponseResult<List<ConfigDeviceDTO>> searchDeviceList(AuthPlatformUserInfo userInfo, String deviceName, Integer status, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigDevice> wrapper = new EntityWrapper<ConfigDevice>()
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
        List<ConfigDevice> deviceList = baseMapper.selectPage(pagination, wrapper);
        List<ConfigDeviceDTO> dto = BeanUtils.assemble(ConfigDeviceDTO.class, deviceList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }
    /**
     * 保存设备信息
     * @param userInfo 用户信息
     * @param rq 设备信息
     * @return 设备id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveDevice(AuthPlatformUserInfo userInfo, ConfigDeviceSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        Integer type = rq.getType();

        // 校验 设备名称+类型 在公司下唯一
        List<ConfigDevice> exist = selectList(new EntityWrapper<ConfigDevice>()
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name)
                .eq("type", type));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存设备信息失败,设备重复,调用{}的{}方法出错", "ConfigDeviceServiceImpl", "saveDevice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DEVICE_SAVE_FAILED_NAME_OR_TYPE_RE);
        }

        ConfigDevice configDevice = BeanUtils.copyProperties(rq, ConfigDevice.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);
        // 保存设备信息
        if (!insert(configDevice)) {
            log.error("保存设备信息失败，调用{}的{}方法出错", "ConfigDeviceServiceImpl", "saveDevice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DEVICE_SAVE_FAILED);

        }
        return configDevice.getId();
    }
    /**
     * 修改设备信息
     * @param userInfo 用户信息
     * @param rq  设备信息
     * @return 设备id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateDevice(AuthPlatformUserInfo userInfo, ConfigDeviceUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        Integer type = rq.getType();
        Integer id = rq.getId();

        // 校验设备是否在使用
        if(checkDeviceUsed(id)){
            log.error("修改设备信息失败,设备已被使用禁止修改,调用{}的{}方法出错", "ConfigDeviceServiceImpl", "updateDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DEVICE_UPDATE_FAILED_USED);
        }

        // 校验 设备名称+类型 在公司下唯一
        List<ConfigDevice> exist = selectList(new EntityWrapper<ConfigDevice>()
                .eq("enterprise_id", enterpriseId)
                .ne("id",id)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name)
                .eq("type", type));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改设备信息失败,设备重复,调用{}的{}方法出错", "ConfigDeviceServiceImpl", "updateDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DEVICE_UPDATE_FAILED_NAME_OR_TYPE_RE);
        }

        // 校验此id的设备数据是否存在
        List<ConfigDevice> have = selectList(new EntityWrapper<ConfigDevice>()
                .eq("enterprise_id", enterpriseId)
                .eq("id",id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改设备信息失败,此id设备不存在,调用{}的{}方法出错", "ConfigDeviceServiceImpl", "updateDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DEVICE_UPDATE_FAILED_NO_DATA);
        }

        ConfigDevice configDevice = BeanUtils.copyProperties(rq, ConfigDevice.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改设备信息
        if (!updateById(configDevice)) {
            log.error("修改设备信息失败，调用{}的{}方法出错", "ConfigDeviceServiceImpl", "updateDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DEVICE_UPDATE_FAILED);

        }
        return configDevice.getId();
    }

    /**
     * 校验设备是否已被使用
     * @return
     */
    private Boolean checkDeviceUsed(Integer id){
        List<ProBlanking> proBlanking = proBlankingService.selectList(new EntityWrapper<ProBlanking>()
                .eq("status", Status.TRUE.getKey())
                .eq("furnace_id",id));
        if(!CollectionUtils.isEmpty(proBlanking)){
            return true;
        }
        return false;
    }


    /**
     * 根据id删除设备信息
     * @param userInfo 用户信息
     * @param id 设备id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteDeviceById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        ConfigDevice exist = selectOne(new EntityWrapper<ConfigDevice>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在设备信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除设备信息，没有找到相应数据，id为："+id);
            return;
        }
        // 校验设备是否在使用
        if(checkDeviceUsed(id)){
            log.error("删除设备失败,设备已被使用禁止删除,调用{}的{}方法出错", "ConfigDeviceServiceImpl", "updateDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DEVICE_DELETED_FAILED_USED);
        }
        // 删除设备信息
        if (!updateById(new ConfigDevice()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除设备信息失败，调用{}的{}方法出错", "ConfigDeviceServiceImpl", "deleteDeviceById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.DEVICE_DELETED_FAILED);
        }
    }

    /**
     * 根据类型查询设备列表 设备类型（0 矿热炉 1 环保设备 2 特种设备 3 其他设备）
     * @param userInfo 用户信息
     * @param types 设备类型
     * @return 设备列表
     */
    @Override
    public List<ConfigDeviceDTO> getDeviceListByType(AuthPlatformUserInfo userInfo, List<Integer> types) {

        Wrapper<ConfigDevice> wrapper = new EntityWrapper<ConfigDevice>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status",Status.TRUE.getKey())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId());
        if(!CollectionUtils.isEmpty(types)){
            wrapper.in("type",types);
        }

        List<ConfigDevice> configDevices = selectList(wrapper);

        return BeanUtils.assemble(ConfigDeviceDTO.class,configDevices);
    }

    /**
     * 根据设备名称模糊搜索设备下拉列表
     * @param userInfo 用户信息
     * @param deviceName 设备名称
     * @return 设备列表
     */
    @Override
    public List<ConfigDeviceDTO> getDeviceListByName(AuthPlatformUserInfo userInfo, String deviceName) {

        Wrapper<ConfigDevice> wrapper = new EntityWrapper<ConfigDevice>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .eq("status", Status.TRUE.getKey())
                .orderBy("id", false);
        if (StringUtils.isNotEmpty(deviceName)) {
            wrapper.like("name", deviceName);
        }

        List<ConfigDevice> deviceList = baseMapper.selectList( wrapper);

        return BeanUtils.assemble(ConfigDeviceDTO.class, deviceList);
    }


    @Override
    public ConfigDeviceDTO getDeviceById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigDevice device = selectOne(new EntityWrapper<ConfigDevice>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(device, ConfigDeviceDTO.class);
    }
}
