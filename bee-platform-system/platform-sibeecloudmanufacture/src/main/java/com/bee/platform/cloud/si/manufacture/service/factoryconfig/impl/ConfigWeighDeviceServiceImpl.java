package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumPrinter;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigPrinterTokenMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigWeighDeviceMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigWeighDeviceDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigPrinterToken;
import com.bee.platform.cloud.si.manufacture.entity.ConfigWeighDevice;
import com.bee.platform.cloud.si.manufacture.rq.ConfigWeighDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigWeighDeviceUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigWeighDeviceService;
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

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 称重设备档案 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigWeighDeviceServiceImpl extends ServiceImpl<ConfigWeighDeviceMapper, ConfigWeighDevice> implements ConfigWeighDeviceService {

    @Autowired
    private ConfigWeighDeviceMapper configWeighDeviceMapper;
    @Autowired
    private ConfigPrinterTokenMapper printerTokenMapper;


    /**
     * 搜索称重设备列表
     *
     * @param userInfo        用户信息
     * @param weighDeviceName 称重设备名称
     * @param status          状态
     * @param page            分页对象
     * @return 称重设备列表
     */
    @Override
    public ResponseResult<List<ConfigWeighDeviceDTO>> searchWeighDeviceList(AuthPlatformUserInfo userInfo, String weighDeviceName, Integer status, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigWeighDevice> wrapper = new EntityWrapper<ConfigWeighDevice>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (StringUtils.isNotEmpty(weighDeviceName)) {
            wrapper.like("name", weighDeviceName);
        }
        if (!ObjectUtils.isEmpty(status)) {
            wrapper.eq("status", status);
        }
        List<ConfigWeighDevice> weighDeviceList = baseMapper.selectPage(pagination, wrapper);
        List<ConfigWeighDeviceDTO> dto = BeanUtils.assemble(ConfigWeighDeviceDTO.class, weighDeviceList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 保存称重设备
     *
     * @param userInfo 用户信息
     * @param rq       称重设备信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveWeighDevice(AuthPlatformUserInfo userInfo, ConfigWeighDeviceSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        String deviceId = rq.getDeviceId();

        // 校验 称重设备名称+id编号 在公司下唯一
        List<ConfigWeighDevice> exist1 = selectList(new EntityWrapper<ConfigWeighDevice>()
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id",factoryId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("device_id", deviceId));
        List<ConfigWeighDevice> exist2 = selectList(new EntityWrapper<ConfigWeighDevice>()
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id",factoryId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name));
        if (!CollectionUtils.isEmpty(exist1) || !CollectionUtils.isEmpty(exist2)) {
            log.error("保存称重设备信息失败,设备重复,调用{}的{}方法出错", "ConfigWeighDeviceServiceImpl", "saveWeighDevice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.WEIGH_DEVICE_SAVE_FAILED_NAME_OR_CODE_RE);
        }

        ConfigWeighDevice configWeighDevice = BeanUtils.copyProperties(rq, ConfigWeighDevice.class).setEnterpriseId(enterpriseId).setFactoryId(factoryId).setCreateId(userId).setCreator(userName).setDeleted(0).setCreateTime(time);
        // 保存称重设备信息
        if (!insert(configWeighDevice)) {
            log.error("保存称重设备信息失败，调用{}的{}方法出错", "ConfigWeighDeviceServiceImpl", "saveWeighDevice()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.WEIGH_DEVICE_SAVE_FAILED);

        }
        // 新增地磅设备 打印机配置表
        if(Status.FALSE.getKey().equals(rq.getType())){
            ConfigPrinterToken printerToken = printerTokenMapper.selectOne(new ConfigPrinterToken()
                    .setEnterpriseId(enterpriseId)
                    .setFactoryId(userInfo.getFactoryId())
                    .setDeviceId(deviceId));
            if(ObjectUtils.isEmpty(printerToken)){
                ConfigPrinterToken printerTokenNew = new ConfigPrinterToken();
                printerTokenNew.setEnterpriseId(enterpriseId);
                printerTokenNew.setFactoryId(userInfo.getFactoryId());
                printerTokenNew.setDeviceId(deviceId);
                printerTokenNew.setPrinterId(EnumPrinter.TEMPLATE_FIRST.PRINTER_ID.getValue());
                printerTokenNew.setAppId(EnumPrinter.TEMPLATE_FIRST.APP_ID.getValue());
                printerTokenNew.setAppKey(EnumPrinter.TEMPLATE_FIRST.APP_KEY.getValue());
                printerTokenNew.setTemplateId(EnumPrinter.TEMPLATE_FIRST.TEMPLATE_ID.getValue());
                printerTokenMapper.insert(printerTokenNew);
            }
        }
        return configWeighDevice.getId();
    }

    /**
     * 修改称重设备
     *
     * @param userInfo 用户信息
     * @param rq       称重设备信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateWeighDevice(AuthPlatformUserInfo userInfo, ConfigWeighDeviceUpdateRQ rq) {

        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        String deviceId = rq.getDeviceId();
        Integer id = rq.getId();

        // 校验称重设备编号在公司下唯一
        List<ConfigWeighDevice> exist1 = selectList(new EntityWrapper<ConfigWeighDevice>()
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id",userInfo.getFactoryId())
                .ne("id", id)
                .eq("deleted", Status.FALSE.getKey())
                .eq("device_id", deviceId));
        List<ConfigWeighDevice> exist2 = selectList(new EntityWrapper<ConfigWeighDevice>()
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id",userInfo.getFactoryId())
                .ne("id", id)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name));
        if (!CollectionUtils.isEmpty(exist1) || !CollectionUtils.isEmpty(exist2)) {
            log.error("修改称重设备信息失败,设备重复,调用{}的{}方法出错", "ConfigWeighDeviceServiceImpl", "updateWeighDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.WEIGH_DEVICE_UPDATE_FAILED_NAME_OR_CODE_RE);
        }

        // 校验此id的称重设备数据是否存在
        List<ConfigWeighDevice> have = selectList(new EntityWrapper<ConfigWeighDevice>()
                .eq("enterprise_id", enterpriseId)
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改称重设备信息失败,此id称重设备不存在,调用{}的{}方法出错", "ConfigWeighDeviceServiceImpl", "updateWeighDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.WEIGH_DEVICE_UPDATE_FAILED_NO_DATA);
        }

        ConfigWeighDevice configWeighDevice = BeanUtils.copyProperties(rq, ConfigWeighDevice.class).setModifyId(userId).setModifier(userName).setModifyTime(time);
        // 修改设备信息
        if (!updateById(configWeighDevice)) {
            log.error("修改称重设备信息失败，调用{}的{}方法出错", "ConfigWeighDeviceServiceImpl", "updateWeighDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.WEIGH_DEVICE_UPDATE_FAILED);

        }
        // 修改打印配置信息[设备id]
        ConfigPrinterToken printerTokenExisted = printerTokenMapper.selectOne(new ConfigPrinterToken()
                .setEnterpriseId(enterpriseId)
                .setFactoryId(userInfo.getFactoryId())
                .setDeviceId(have.get(0).getDeviceId()));
        if(!ObjectUtils.isEmpty(printerTokenExisted)){
            printerTokenExisted.setDeviceId(rq.getDeviceId());
            printerTokenMapper.updateById(printerTokenExisted);
        }
        return configWeighDevice.getId();
    }


    /**
     * 根据id删除称重设备
     *
     * @param userInfo 用户信息
     * @param id       称重设备id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteWeighDeviceById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        ConfigWeighDevice exist = selectOne(new EntityWrapper<ConfigWeighDevice>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在称重设备信息
        if (ObjectUtils.isEmpty(exist)) {
            log.info("根据id删除称重设备信息，没有找到相应数据，id为：" + id);
            return;
        }

        // 删除称重设备信息
        if (!updateById(new ConfigWeighDevice()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除称重设备信息失败，调用{}的{}方法出错", "ConfigWeighDeviceServiceImpl", "deleteWeighDeviceById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.WEIGH_DEVICE_DELETED_FAILED);
        }
        // 删除打印配置信息
        ConfigPrinterToken printerToken = printerTokenMapper.selectOne(new ConfigPrinterToken()
                .setDeviceId(exist.getDeviceId())
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId()));
        if(!ObjectUtils.isEmpty(printerToken)){
            printerTokenMapper.deleteById(printerToken.getId());
        }
    }

    /**
     * @Description 获取称重设备信息
     * @author chenxm66777123
     * @Date 2019/9/27 9:54
     * @version 1.0.0
     */
    @Override
    public List<ConfigWeighDeviceDTO> getWeightSelectList(AuthPlatformUserInfo userInfo, Integer type) {
        //根据企业和类型来查询出需要显示的数据
        List<ConfigWeighDevice> configWeighDevices = configWeighDeviceMapper.
                selectList(new EntityWrapper<ConfigWeighDevice>()
                .where("deleted = 0 and enterprise_id ={0} and type = {1}", userInfo.getOrgId(), type));

        //copy
        List<ConfigWeighDeviceDTO> configWeighDeviceDTOList
                = BeanUtils.assemble(ConfigWeighDeviceDTO.class, configWeighDevices);

        return configWeighDeviceDTOList;
    }


    @Override
    public ConfigWeighDeviceDTO getWeighDeviceById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigWeighDevice dto = selectOne(new EntityWrapper<ConfigWeighDevice>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(dto, ConfigWeighDeviceDTO.class);
    }


    @Override
    public List<ConfigWeighDeviceDTO> getWeighDeviceListByType(AuthPlatformUserInfo userInfo, List<Integer> types) {
        Wrapper<ConfigWeighDevice> wrapper = new EntityWrapper<ConfigWeighDevice>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status",Status.TRUE.getKey())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId());
        if(!CollectionUtils.isEmpty(types)){
            wrapper.in("type",types);
        }

        List<ConfigWeighDevice> weighDevices = selectList(wrapper);

        return BeanUtils.assemble(ConfigWeighDeviceDTO.class,weighDevices);
    }
}
