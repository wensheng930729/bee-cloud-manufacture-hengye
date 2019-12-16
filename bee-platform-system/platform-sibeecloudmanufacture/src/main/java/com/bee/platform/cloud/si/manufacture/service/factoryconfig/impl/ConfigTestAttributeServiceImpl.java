package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigTestAttributeMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigDeviceDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigTestAttributeDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigDevice;
import com.bee.platform.cloud.si.manufacture.entity.ConfigTestAttribute;
import com.bee.platform.cloud.si.manufacture.rq.ConfigSearchTypeListRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigTestAttributeSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigTestAttributeSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigTestAttributeUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigTestAttributeService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
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
 * 化验属性配置表 服务实现类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@Slf4j
@Service
public class ConfigTestAttributeServiceImpl extends ServiceImpl<ConfigTestAttributeMapper, ConfigTestAttribute> implements ConfigTestAttributeService {

    /**
     * 根据条件搜索化验属性列表
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return 化验属性列表
     */
    @Override
    public List<ConfigTestAttributeDTO> searchTestAttributeList(AuthPlatformUserInfo userInfo, ConfigTestAttributeSearchRQ rq) {

        Wrapper<ConfigTestAttribute> wrapper = new EntityWrapper<ConfigTestAttribute>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if(!StringUtils.isEmpty(rq.getAttributeName())){
            wrapper.like("attribute_name",rq.getAttributeName());
        }

        if(!StringUtils.isEmpty(rq.getStartTime())) {
            wrapper.gt("create_time",rq.getStartTime());
        }

        if(!StringUtils.isEmpty( rq.getEndTime())) {
            wrapper.lt("create_time",rq.getEndTime());
        }

        List<ConfigTestAttribute> testAttributes = selectList(wrapper);

        return BeanUtils.assemble(ConfigTestAttributeDTO.class,testAttributes);
    }

    /**
     * 保存化验属性
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return 化验属性id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveTestAttribute(AuthPlatformUserInfo userInfo, ConfigTestAttributeSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String attributeName = rq.getAttributeName().replaceAll("\\s*", "");
        rq.setAttributeName(attributeName);
        Integer type = rq.getType();

        // 校验 化验属性名称+类型 在公司工厂下唯一
        List<ConfigTestAttribute> exist = selectList(new EntityWrapper<ConfigTestAttribute>()
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id",factoryId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("attribute_name", attributeName)
                .eq("type", type));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存化验属性失败,化验属性重复,调用{}的{}方法出错", "ConfigTestAttributeServiceImpl", "saveTestAttribute()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.TEST_ATTRIBUTE_SAVE_FAILED_NAME_OR_TYPE_RE);
        }

        ConfigTestAttribute testAttribute = BeanUtils.copyProperties(rq, ConfigTestAttribute.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);
        // 保存化验属性信息
        if (!insert(testAttribute)) {
            log.error("保存化验属性信息失败，调用{}的{}方法出错", "ConfigTestAttributeServiceImpl", "saveTestAttribute()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.TEST_ATTRIBUTE_SAVE_FAILED);

        }
        return testAttribute.getId();




    }

    /**
     * 修改化验属性
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return 化验属性id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateTestAttribute(AuthPlatformUserInfo userInfo, ConfigTestAttributeUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String attributeName = rq.getAttributeName().replaceAll("\\s*", "");
        rq.setAttributeName(attributeName);
        Integer type = rq.getType();
        Integer id = rq.getId();


        // 校验 化验属性名称+类型 在公司工厂下唯一
        List<ConfigTestAttribute> exist = selectList(new EntityWrapper<ConfigTestAttribute>()
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id",factoryId)
                .ne("id",id)
                .eq("deleted", Status.FALSE.getKey())
                .eq("attribute_name", attributeName)
                .eq("type", type));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改化验属性失败,化验属性重复,调用{}的{}方法出错", "ConfigTestAttributeServiceImpl", "updateTestAttribute()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.TEST_ATTRIBUTE_UPDATE_FAILED_NAME_OR_TYPE_RE);
        }

        // 校验此id的化验属性是否存在
        List<ConfigTestAttribute> have = selectList(new EntityWrapper<ConfigTestAttribute>()
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id",factoryId)
                .eq("id",id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改化验属性失败,此id化验属性不存在,调用{}的{}方法出错", "ConfigTestAttributeServiceImpl", "updateTestAttribute()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.TEST_ATTRIBUTE_UPDATE_FAILED_NO_DATA);
        }

        ConfigTestAttribute testAttribute = BeanUtils.copyProperties(rq, ConfigTestAttribute.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改化验属性
        if (!updateById(testAttribute)) {
            log.error("修改化验属性失败，调用{}的{}方法出错", "ConfigTestAttributeServiceImpl", "updateTestAttribute()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.TEST_ATTRIBUTE_UPDATE_FAILED);

        }
        return id;
    }


    /**
     * 根据id删除化验属性
     * @param userInfo 用户信息
     * @param id id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteTestAttributeById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        ConfigTestAttribute exist = selectOne(new EntityWrapper<ConfigTestAttribute>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("factory_id",factoryId)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在化验属性
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除化验属性，没有找到相应数据，id为："+id);
            return;
        }

        // 删除化验属性
        if (!updateById(new ConfigTestAttribute()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除化验属性失败，调用{}的{}方法出错", "ConfigTestAttributeServiceImpl", "deleteTestAttributeById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.TEST_ATTRIBUTE_DELETED_FAILED);
        }
    }


    @Override
    public List<ConfigTestAttributeDTO> getTestAttributeByType(AuthPlatformUserInfo userInfo, List<Integer> types) {
        Wrapper<ConfigTestAttribute> wrapper = new EntityWrapper<ConfigTestAttribute>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId());
        if(!CollectionUtils.isEmpty(types)){
            wrapper.in("type",types);
        }

        List<ConfigTestAttribute> list = selectList(wrapper);

        return BeanUtils.assemble(ConfigTestAttributeDTO.class,list);
    }
}
