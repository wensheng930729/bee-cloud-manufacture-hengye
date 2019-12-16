package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigRawMaterialLossMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigRawMaterialLossDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigRawMaterialLoss;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRawMaterialLossSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRawMaterialLossUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRawMaterialLossService;
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
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 原料损耗配置表 服务实现类
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@Slf4j
@Service
public class ConfigRawMaterialLossServiceImpl extends ServiceImpl<ConfigRawMaterialLossMapper, ConfigRawMaterialLoss> implements ConfigRawMaterialLossService {

    /**
     * 条件查询原料损耗列表
     * @param userInfo 用户信息
     * @param productName 产品名称
     * @param page 分页对象
     * @return 原料损耗列表
     */
    @Override
    public ResponseResult<List<ConfigRawMaterialLossDTO>> searchRawMaterialLossList(AuthPlatformUserInfo userInfo, String productName, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigRawMaterialLoss> wrapper = new EntityWrapper<ConfigRawMaterialLoss>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (StringUtils.isNotEmpty(productName)) {
            wrapper.like("product_name", productName);
        }

        List<ConfigRawMaterialLoss> list = baseMapper.selectPage(pagination, wrapper);
        List<ConfigRawMaterialLossDTO> dto = BeanUtils.assemble(ConfigRawMaterialLossDTO.class, list);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }
    /**
     * 保存原料损耗信息
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    @Override
    public Integer saveRawMaterialLoss(AuthPlatformUserInfo userInfo, ConfigRawMaterialLossSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Integer productId = rq.getProductId();

        // 校验 产品名称重复
        List<ConfigRawMaterialLoss> exist = selectList(new EntityWrapper<ConfigRawMaterialLoss>()
                .eq("product_id", productId)
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存原料损耗信息失败,产品重复,调用{}的{}方法出错", "ConfigRawMaterialLossServiceImpl", "saveRawMaterialLoss()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.RAW_MATERIAL_LOSS_SAVE_FAILED_NAME_RE);
        }

        ConfigRawMaterialLoss save = BeanUtils.copyProperties(rq, ConfigRawMaterialLoss.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);
        // 保存原料损耗信息
        if (!insert(save)) {
            log.error("保存原料损耗信息失败，调用{}的{}方法出错", "ConfigRawMaterialLossServiceImpl", "saveRawMaterialLoss()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.RAW_MATERIAL_LOSS_SAVE_FAILED);

        }
        return save.getId();
    }
    /**
     * 修改原料损耗信息
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    @Override
    public Integer updateRawMaterialLoss(AuthPlatformUserInfo userInfo, ConfigRawMaterialLossUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer productId = rq.getProductId();
        Integer id = rq.getId();
        // 校验 产品名称重复
        List<ConfigRawMaterialLoss> exist = selectList(new EntityWrapper<ConfigRawMaterialLoss>()
                .ne("id",id)
                .eq("product_id", productId)
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey()));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改原料损耗信息失败,产品重复,调用{}的{}方法出错", "ConfigRawMaterialLossServiceImpl", "updateRawMaterialLoss()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.RAW_MATERIAL_LOSS_UPDATE_FAILED_NAME_RE);
        }

        ConfigRawMaterialLoss update = BeanUtils.copyProperties(rq, ConfigRawMaterialLoss.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改原料损耗信息
        if (!updateById(update)) {
            log.error("修改原料损耗信息失败，调用{}的{}方法出错", "ConfigRawMaterialLossServiceImpl", "updateRawMaterialLoss()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.RAW_MATERIAL_LOSS_UPDATE_FAILED);

        }
        return update.getId();
    }
    /**
     * 根据id删除原料损耗信息
     * @param userInfo 用户信息
     * @param id id
     */
    @Override
    public void deleteRawMaterialLossById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        ConfigRawMaterialLoss exist = selectOne(new EntityWrapper<ConfigRawMaterialLoss>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在原料损耗信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除原料损耗信息，没有找到相应数据，id为："+id);
            return;
        }
        // 删除原料损耗信息
        if (!updateById(new ConfigRawMaterialLoss()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除原料损耗信息失败，调用{}的{}方法出错", "ConfigRawMaterialLossServiceImpl", "deleteRawMaterialLossById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.RAW_MATERIAL_LOSS_DELETED_FAILED);
        }
    }
    /**
     * 根据id查询原料损耗信息
     * @param userInfo 用户信息
     * @param id id
     * @return 原料损耗信息
     */
    @Override
    public ConfigRawMaterialLossDTO getRawMaterialLossById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigRawMaterialLoss one = selectOne(new EntityWrapper<ConfigRawMaterialLoss>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(one, ConfigRawMaterialLossDTO.class);

    }
}
