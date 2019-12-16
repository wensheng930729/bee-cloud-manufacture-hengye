package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigLocationMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigLocationDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigLocation;
import com.bee.platform.cloud.si.manufacture.rq.ConfigLocationSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigLocationUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigLocationService;
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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 物流地点管理表 服务实现类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */

@Slf4j
@Service
public class ConfigLocationServiceImpl extends ServiceImpl<ConfigLocationMapper, ConfigLocation> implements ConfigLocationService {

    /**
     * 条件查询物流地点列表
     * @param userInfo 用户信息
     * @param name 地点名称
     * @param page 分页对象
     * @return 地点列表
     */
    @Override
    public ResponseResult<List<ConfigLocationDTO>> searchLocationList(AuthPlatformUserInfo userInfo, String name, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();

        Wrapper<ConfigLocation> wrapper = new EntityWrapper<ConfigLocation>()
                .eq("enterprise_id",enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (!ObjectUtils.isEmpty(factoryId)) {
            wrapper.eq("factory_id", factoryId);
        }
        if (!StringUtils.isEmpty(name)) {
            wrapper.like("name", name);
        }

        List<ConfigLocation> locationList = baseMapper.selectPage(pagination, wrapper);
        List<ConfigLocationDTO> dto = BeanUtils.assemble(ConfigLocationDTO.class, locationList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     *
     * 保存物流地点信息
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveLocation(AuthPlatformUserInfo userInfo, ConfigLocationSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);

        // 校验 地点名称 在公司工厂下唯一
        Wrapper<ConfigLocation> wrapper = new EntityWrapper<ConfigLocation>()
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name);

        if (!ObjectUtils.isEmpty(factoryId)) {
            wrapper.eq("factory_id", factoryId);
        }

        List<ConfigLocation> exist = selectList(wrapper);
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存物流地点失败,物流地点重复,调用{}的{}方法出错", "ConfigLocationServiceImpl", "saveLocation()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.LOCATION_SAVE_FAILED_NAME_RE);
        }

        ConfigLocation location = BeanUtils.copyProperties(rq, ConfigLocation.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);
        // 保存物流地点信息
        if (!insert(location)) {
            log.error("保存物流地点信息失败，调用{}的{}方法出错", "ConfigLocationServiceImpl", "saveLocation()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.LOCATION_SAVE_FAILED);

        }
        return location.getId();
    }

    /**
     * 修改物流地点
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateLocation(AuthPlatformUserInfo userInfo, ConfigLocationUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        Integer id = rq.getId();

        // 校验 物流地点 在公司工厂下唯一
        Wrapper<ConfigLocation> wrapper = new EntityWrapper<ConfigLocation>()
                .eq("enterprise_id", enterpriseId)
                .ne("id", id)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name);
        if (!ObjectUtils.isEmpty(factoryId)) {
            wrapper.eq("factory_id", factoryId);
        }

        List<ConfigLocation> exist = selectList(wrapper);
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改物流地点失败,物流地点重复,调用{}的{}方法出错", "ConfigLocationServiceImpl", "updateLocation()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.LOCATION_SAVE_FAILED_NAME_RE);
        }

        // 校验此id的物流地点是否存在
        List<ConfigLocation> have = selectList(new EntityWrapper<ConfigLocation>()
                .eq("enterprise_id", enterpriseId)
                .eq("id",id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改物流地点失败,此id物流地点不存在,调用{}的{}方法出错", "ConfigLocationServiceImpl", "updateLocation()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.LOCATION_UPDATE_FAILED_NO_DATA);
        }

        ConfigLocation update = BeanUtils.copyProperties(rq, ConfigLocation.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改设备信息
        if (!updateById(update)) {
            log.error("修改物流地点失败，调用{}的{}方法出错", "ConfigLocationServiceImpl", "updateLocation()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.LOCATION_UPDATE_FAILED);

        }
        return id;
    }


    /**
     * 根据id删除物流地点
     * @param userInfo 用户信息
     * @param id id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteLocationById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        ConfigLocation exist = selectOne(new EntityWrapper<ConfigLocation>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在物流地点信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除物流地点信息，没有找到相应数据，id为："+id);
            return;
        }

        // 删除物流地点信息
        if (!updateById(new ConfigLocation()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除物流地点信息失败，调用{}的{}方法出错", "ConfigLocationServiceImpl", "deleteLocationById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.LOCATION_DELETED_FAILED);
        }
    }


    /**
     * 根据id查询地点详情
     * @param userInfo 用户信息
     * @param id id
     * @return 地点详情
     */
    @Override
    public ConfigLocationDTO getLocationById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        ConfigLocation detail = selectOne(new EntityWrapper<ConfigLocation>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(detail, ConfigLocationDTO.class);
    }


    @Override
    public List<ConfigLocationDTO> getLocationList(AuthPlatformUserInfo userInfo) {

        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();

        Wrapper<ConfigLocation> wrapper = new EntityWrapper<ConfigLocation>()
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("status",Status.TRUE.getKey())
                .orderBy("id", false);

        if (!ObjectUtils.isEmpty(factoryId)) {
            wrapper.eq("factory_id", factoryId);
        }

        List<ConfigLocation> locationList = baseMapper.selectList( wrapper);
        List<ConfigLocationDTO> dto = BeanUtils.assemble(ConfigLocationDTO.class, locationList);

        return dto;
    }
}
