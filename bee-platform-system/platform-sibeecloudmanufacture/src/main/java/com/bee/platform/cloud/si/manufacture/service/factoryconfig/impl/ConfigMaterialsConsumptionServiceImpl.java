package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigMaterialsConsumptionMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigMaterialsConsumptionDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigMaterialsConsumption;
import com.bee.platform.cloud.si.manufacture.rq.ConfigMaterialsConsumptionSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigMaterialsConsumptionUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigMaterialsConsumptionService;
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

import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 原料吨耗 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigMaterialsConsumptionServiceImpl extends ServiceImpl<ConfigMaterialsConsumptionMapper, ConfigMaterialsConsumption> implements ConfigMaterialsConsumptionService {

    /**
     * 条件搜索原料吨耗列表
     * @param userInfo 用户信息
     * @param productName 产品名称
     * @param page 分页对象
     * @return 原料吨耗信息
     */
    @Override
    public ResponseResult<List<ConfigMaterialsConsumptionDTO>> searchMaterialsConsumptionList(AuthPlatformUserInfo userInfo, String productName, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigMaterialsConsumption> wrapper = new EntityWrapper<ConfigMaterialsConsumption>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (StringUtils.isNotEmpty(productName)) {
            wrapper.like("product_name", productName);
        }

        List<ConfigMaterialsConsumption> deviceList = baseMapper.selectPage(pagination, wrapper);
        List<ConfigMaterialsConsumptionDTO> dto = BeanUtils.assemble(ConfigMaterialsConsumptionDTO.class, deviceList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }
    /**
     * 保存原料吨耗信息
     * @param userInfo 用户信息
     * @param rq 原料吨耗信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveMaterialsConsumption(AuthPlatformUserInfo userInfo, ConfigMaterialsConsumptionSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Integer productId = rq.getProductId();

        // 校验 产品名称重复
        List<ConfigMaterialsConsumption> exist = selectList(new EntityWrapper<ConfigMaterialsConsumption>()
                .eq("product_id", productId)
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存原料吨耗信息失败,产品重复,调用{}的{}方法出错", "ConfigMaterialsConsumptionServiceImpl", "saveMaterialsConsumption()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.MATERIALS_CONSUMPTION_SAVE_FAILED_NAME_RE);
        }

        ConfigMaterialsConsumption configMaterialsConsumption = BeanUtils.copyProperties(rq, ConfigMaterialsConsumption.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);
        // 保存原料吨耗信息
        if (!insert(configMaterialsConsumption)) {
            log.error("保存原料吨耗信息失败，调用{}的{}方法出错", "ConfigMaterialsConsumptionServiceImpl", "saveMaterialsConsumption()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.MATERIALS_CONSUMPTION_SAVE_FAILED);

        }
        return configMaterialsConsumption.getId();
    }
    /**
     * 修改原料吨耗信息
     * @param userInfo 用户信息
     * @param rq 原料吨耗信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateMaterialsConsumption(AuthPlatformUserInfo userInfo, ConfigMaterialsConsumptionUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer productId = rq.getProductId();
        Integer id = rq.getId();

        // 校验 产品名称重复
        List<ConfigMaterialsConsumption> exist = selectList(new EntityWrapper<ConfigMaterialsConsumption>()
                .ne("id",id)
                .eq("product_id", productId)
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改原料吨耗信息失败,产品重复,调用{}的{}方法出错", "ConfigMaterialsConsumptionServiceImpl", "updateMaterialsConsumption()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.MATERIALS_CONSUMPTION_UPDATE_FAILED_NAME_RE);
        }


        ConfigMaterialsConsumption configMaterialsConsumption = BeanUtils.copyProperties(rq, ConfigMaterialsConsumption.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改原料吨耗信息
        if (!updateById(configMaterialsConsumption)) {
            log.error("修改原料吨耗信息失败，调用{}的{}方法出错", "ConfigMaterialsConsumptionServiceImpl", "updateMaterialsConsumption()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.MATERIALS_CONSUMPTION_UPDATE_FAILED);

        }
        return configMaterialsConsumption.getId();
    }
    /**
     * 根据id删除原料吨耗信息
     * @param userInfo 用户信息
     * @param id id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteMaterialsConsumptionById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        ConfigMaterialsConsumption exist = selectOne(new EntityWrapper<ConfigMaterialsConsumption>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在原料吨耗信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除原料吨耗信息，没有找到相应数据，id为："+id);
            return;
        }
        // 删除原料吨耗信息
        if (!updateById(new ConfigMaterialsConsumption()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除原料吨耗信息失败，调用{}的{}方法出错", "ConfigMaterialsConsumptionServiceImpl", "deleteMaterialsConsumptionById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.MATERIALS_CONSUMPTION_DELETED_FAILED);
        }
    }


    @Override
    public ConfigMaterialsConsumptionDTO getMaterialsConsumptionById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigMaterialsConsumption materialsConsumption = selectOne(new EntityWrapper<ConfigMaterialsConsumption>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(materialsConsumption, ConfigMaterialsConsumptionDTO.class);
    }
}
