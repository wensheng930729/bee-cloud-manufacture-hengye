package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ConfigAmmeterDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigDeviceInspectionDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigAmmeter;
import com.bee.platform.cloud.si.manufacture.entity.ConfigDeviceInspection;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigDeviceInspectionMapper;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceInspectionSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceInspectionUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigDeviceInspectionService;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 设备巡检 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigDeviceInspectionServiceImpl extends ServiceImpl<ConfigDeviceInspectionMapper, ConfigDeviceInspection> implements ConfigDeviceInspectionService {


    /**
     * 搜索设备巡检信息列表
     * @param userInfo 用户信息
     * @param deviceName 设备名称
     * @param page 分页对象
     * @return 设备巡检列表
     */
    @Override
    public ResponseResult<List<ConfigDeviceInspectionDTO>> searchDeviceInspectionList(AuthPlatformUserInfo userInfo, String deviceName, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigDeviceInspection> wrapper = new EntityWrapper<ConfigDeviceInspection>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (StringUtils.isNotEmpty(deviceName)) {
            wrapper.like("name", deviceName);
        }
        List<ConfigDeviceInspection> deviceInspectionList = baseMapper.selectPage(pagination, wrapper);
        List<ConfigDeviceInspectionDTO> dto = BeanUtils.assemble(ConfigDeviceInspectionDTO.class, deviceInspectionList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }


    /**
     * 保存设备巡检信息
     * @param userInfo 用户信息
     * @param rq 设备巡检信息
     * @return 设备巡检id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveDeviceInspection(AuthPlatformUserInfo userInfo, ConfigDeviceInspectionSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String code = rq.getCode().replaceAll("\\s*", "");
        rq.setCode(code);
        // 校验设备编号在公司下唯一
        List<ConfigDeviceInspection> exist = selectList(new EntityWrapper<ConfigDeviceInspection>()
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("code", code));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存设备巡检信息失败,设备编号重复,调用{}的{}方法出错", "ConfigDeviceInspectionServiceImpl", "saveDeviceInspection()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DEVICE_INSPECTION_SAVE_FAILED_CODE_RE);
        }

        ConfigDeviceInspection configDeviceInspection = BeanUtils.copyProperties(rq, ConfigDeviceInspection.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setDeleted(Status.FALSE.getKey())
                .setCreator(userName)
                .setCreateTime(time);
        // 保存设备巡检信息
        if (!insert(configDeviceInspection)) {
            log.error("保存设备巡检信息失败，调用{}的{}方法出错", "ConfigDeviceInspectionServiceImpl", "saveDeviceInspection()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DEVICE_INSPECTION_SAVE_FAILED);

        }
        return configDeviceInspection.getId();
    }


    /**
     * 修改设备巡检信息
     * @param userInfo  用户信息
     * @param rq 设备巡检信息
     * @return 设备巡检id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateDeviceInspection(AuthPlatformUserInfo userInfo, ConfigDeviceInspectionUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        String code = rq.getCode().replaceAll("\\s*", "");
        rq.setCode(code);
        Integer id = rq.getId();
        // 校验设备编号在公司下唯一
        List<ConfigDeviceInspection> exist = selectList(new EntityWrapper<ConfigDeviceInspection>()
                .eq("enterprise_id", enterpriseId)
                .ne("id",id)
                .eq("deleted", Status.FALSE.getKey())
                .eq("code", code));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改设备巡检信息失败,设备编号重复,调用{}的{}方法出错", "ConfigDeviceInspectionServiceImpl", "updateDeviceInspection()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DEVICE_INSPECTION_UPDATE_FAILED_CODE_RE);
        }

        // 校验此id的设备巡检数据是否存在
        List<ConfigDeviceInspection> have = selectList(new EntityWrapper<ConfigDeviceInspection>()
                .eq("enterprise_id", enterpriseId)
                .eq("id",id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改设备巡检信息失败,此id设备巡检不存在,调用{}的{}方法出错", "ConfigDeviceInspectionServiceImpl", "updateDeviceInspection()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DEVICE_INSPECTION_UPDATE_FAILED_NO_DATA);
        }

        ConfigDeviceInspection configDeviceInspection = BeanUtils.copyProperties(rq, ConfigDeviceInspection.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改设备信息
        if (!updateById(configDeviceInspection)) {
            log.error("修改设备巡检信息失败，调用{}的{}方法出错", "ConfigDeviceInspectionServiceImpl", "updateDeviceInspection()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DEVICE_INSPECTION_UPDATE_FAILED);

        }
        return configDeviceInspection.getId();
    }


    /**
     * 根据id删除设备巡检信息
     * @param userInfo 用户信息
     * @param id 设备巡检id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteDeviceInspectionById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        ConfigDeviceInspection exist = selectOne(new EntityWrapper<ConfigDeviceInspection>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在电表信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除设备巡检信息，没有找到相应数据，id为："+id);
            return;
        }
        // 删除电表信息
        if (!updateById(new ConfigDeviceInspection()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除设备巡检信息失败，调用{}的{}方法出错", "ConfigDeviceInspectionServiceImpl", "deleteDeviceInspectionById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.DEVICE_INSPECTION_DELETED_FAILED);
        }
    }

    /**
     * 根据设备巡检编号查询是否存在设备巡检信息
     * @param userInfo 用户信息
     * @param deviceCode 设备编号
     * @return 结果 true 有数据 false  无数据
     */
    @Override
    public Boolean checkDeviceInspection(AuthPlatformUserInfo userInfo, String deviceCode) {

        List<ConfigDeviceInspection> deviceInspections = selectList(new EntityWrapper<ConfigDeviceInspection>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("code", deviceCode)
                .eq("deleted", Status.FALSE.getKey()));

        if(CollectionUtils.isEmpty(deviceInspections)){
            return false;
        }
        return true;
    }

    /**
     * 根据设备编号查询设备巡检信息
     * @param userInfo 用户信息
     * @param deviceCode 设备编号
     * @return 设备巡检信息
     */
    @Override
    public ConfigDeviceInspectionDTO getDeviceInspectionByCode(AuthPlatformUserInfo userInfo, String deviceCode) {

        ConfigDeviceInspection deviceInspections = selectOne(new EntityWrapper<ConfigDeviceInspection>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("code", deviceCode)
                .eq("deleted", Status.FALSE.getKey()));

        return BeanUtils.copyProperties(deviceInspections,ConfigDeviceInspectionDTO.class);
    }


    @Override
    public ConfigDeviceInspectionDTO getDeviceInspectionById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigDeviceInspection deviceInspection = selectOne(new EntityWrapper<ConfigDeviceInspection>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(deviceInspection, ConfigDeviceInspectionDTO.class);
    }
}
