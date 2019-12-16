package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigAmmeterMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigAmmeterDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigAmmeter;
import com.bee.platform.cloud.si.manufacture.rq.ConfigAmmeterSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigAmmeterUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigAmmeterService;
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
 * 电表档案 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigAmmeterServiceImpl extends ServiceImpl<ConfigAmmeterMapper, ConfigAmmeter> implements ConfigAmmeterService {

    /**
     * 根据电表名称搜索电表列表
     *
     * @param userInfo    用户信息
     * @param ammeterName 电表名称
     * @param page        分页对象
     * @return 电表列表
     */
    @Override
    public ResponseResult<List<ConfigAmmeterDTO>> searchAmmeterList(AuthPlatformUserInfo userInfo, String ammeterName, Page page) {
        String factoryName = userInfo.getFactoryName();
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigAmmeter> wrapper = new EntityWrapper<ConfigAmmeter>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (StringUtils.isNotEmpty(ammeterName)) {
            wrapper.like("name", ammeterName);
        }
        List<ConfigAmmeter> ammeterList = baseMapper.selectPage(pagination, wrapper);
        List<ConfigAmmeterDTO> dto = BeanUtils.assemble(ConfigAmmeterDTO.class, ammeterList);
        dto.forEach(o->o.setFactoryName(factoryName));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
    }


    /**
     * 保存电表信息
     *
     * @param userInfo 用户信息
     * @param rq       电表信息
     * @return 电表id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveAmmeter(AuthPlatformUserInfo userInfo, ConfigAmmeterSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        String code = rq.getCode().replaceAll("\\s*", "");
        rq.setCode(code);
        // 校验电表编号在公司下唯一
        List<ConfigAmmeter> exist = selectList(new EntityWrapper<ConfigAmmeter>()
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("code", code));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存电表信息失败,电表编号重复,调用{}的{}方法出错", "ConfigAmmeterServiceImpl", "saveAmmeter()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.AMMETER_SAVE_FAILED_CODE_RE);
        }

        ConfigAmmeter configAmmeter = BeanUtils.copyProperties(rq, ConfigAmmeter.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setCreateTime(time)
                .setDeleted(Status.FALSE.getKey());
        // 保存电表信息
        if (!insert(configAmmeter)) {
            log.error("保存电表信息失败，调用{}的{}方法出错", "ConfigAmmeterServiceImpl", "saveAmmeter()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.AMMETER_SAVE_FAILED);

        }
        return configAmmeter.getId();
    }

    /**
     * 修改电表信息
     * @param userInfo 用户信息
     * @param rq 电表信息
     * @return 电表id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateAmmeter(AuthPlatformUserInfo userInfo, ConfigAmmeterUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        String code = rq.getCode().replaceAll("\\s*", "");
        rq.setCode(code);
        Integer id = rq.getId();
        // 校验电表编号在公司下唯一
        List<ConfigAmmeter> exist = selectList(new EntityWrapper<ConfigAmmeter>()
                .eq("enterprise_id", enterpriseId)
                .ne("id",id)
                .eq("deleted", Status.FALSE.getKey())
                .eq("code", code));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改电表信息失败,电表编号重复,调用{}的{}方法出错", "ConfigAmmeterServiceImpl", "updateAmmeter()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.AMMETER_UPDATE_FAILED_CODE_RE);
        }
        // 校验此id的电表数据是否存在
        List<ConfigAmmeter> have = selectList(new EntityWrapper<ConfigAmmeter>()
                .eq("enterprise_id", enterpriseId)
                .eq("id",id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改电表信息失败,此id电表不存在,调用{}的{}方法出错", "ConfigAmmeterServiceImpl", "updateAmmeter()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.AMMETER_UPDATE_FAILED_NO_DATA);
        }

        ConfigAmmeter configAmmeter = BeanUtils.copyProperties(rq, ConfigAmmeter.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改电表信息
        if (!updateById(configAmmeter)) {
            log.error("修改电表信息失败，调用{}的{}方法出错", "ConfigAmmeterServiceImpl", "updateAmmeter()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.AMMETER_UPDATE_FAILED);

        }
        return configAmmeter.getId();
    }

    /**
     * 根据id删除电表信息
     * @param userInfo 用户信息
     * @param id id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteAmmeterById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        ConfigAmmeter exist = selectOne(new EntityWrapper<ConfigAmmeter>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在电表信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除电表信息，没有找到相应数据，id为："+id);
            return;
        }
        // 删除电表信息
        if (!updateById(new ConfigAmmeter()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除电表信息失败，调用{}的{}方法出错", "ConfigAmmeterServiceImpl", "deleteAmmeterById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.AMMETER_DELETED_FAILED);
        }
    }


    @Override
    public ConfigAmmeterDTO getAmmeterById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigAmmeter ammeter = selectOne(new EntityWrapper<ConfigAmmeter>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(ammeter, ConfigAmmeterDTO.class);
    }


    @Override
    public List<ConfigAmmeterDTO> getAmmeterList(AuthPlatformUserInfo userInfo) {
        Wrapper<ConfigAmmeter> wrapper = new EntityWrapper<ConfigAmmeter>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);

        List<ConfigAmmeter> ammeterList = selectList(wrapper);
        List<ConfigAmmeterDTO> dto = BeanUtils.assemble(ConfigAmmeterDTO.class, ammeterList);

        return dto;
    }
}
