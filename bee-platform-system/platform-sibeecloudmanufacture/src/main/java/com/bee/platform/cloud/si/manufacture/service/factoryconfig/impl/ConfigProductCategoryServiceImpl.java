package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigProductCategoryMapper;
import com.bee.platform.cloud.si.manufacture.dto.ConfigProductCategoryDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigProduct;
import com.bee.platform.cloud.si.manufacture.entity.ConfigProductCategory;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategorySaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategorySearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductCategoryUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductCategoryService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductService;
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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 产品类别 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */

@Slf4j
@Service
public class ConfigProductCategoryServiceImpl extends ServiceImpl<ConfigProductCategoryMapper, ConfigProductCategory> implements ConfigProductCategoryService {

    @Autowired
    private ConfigProductService productService;

    /**
     * 搜索产品类别列表
     *
     * @param userInfo 用户信息
     * @param rq       产品名称
     * @param page     分页对象
     * @return 产品分类列表
     */
    @Override
    public ResponseResult<List<ConfigProductCategoryDTO>> searchProductCategoryList(AuthPlatformUserInfo userInfo, ConfigProductCategorySearchRQ rq, Page page) {

        Pagination pagination = PageUtils.transFromPage(page);
        rq.setEnterpriseId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());

        List<ConfigProductCategoryDTO> dto = baseMapper.selectListByCondition(pagination, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 保存产品类别
     *
     * @param userInfo 用户信息
     * @param rq       产品类别信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveProductCategory(AuthPlatformUserInfo userInfo, ConfigProductCategorySaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        // 查询默认产品分类名称
        List<ConfigProductCategory> defaultCategory = selectList(new EntityWrapper<ConfigProductCategory>()
                .eq("type", 0)
                .eq("deleted", Status.FALSE.getKey()));
        List<String> defaultNames = defaultCategory.stream().map(o -> o.getName()).distinct().collect(Collectors.toList());

        // 校验 分类名称不能与默认的分类名称相同
        if (defaultNames.contains(name)) {
            log.error("保存产品分类信息失败,产品分类名称重复,调用{}的{}方法出错", "ConfigProductCategoryServiceImpl", "saveProductCategory()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_SAVE_FAILED_NAME_RE);
        }

        // 校验 产品分类名称 在公司下唯一
        List<ConfigProductCategory> exist = selectList(new EntityWrapper<ConfigProductCategory>()
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存产品分类信息失败,产品分类名称重复,调用{}的{}方法出错", "ConfigProductCategoryServiceImpl", "saveProductCategory()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_SAVE_FAILED_NAME_RE);
        }

        ConfigProductCategory configProductCategory = BeanUtils.copyProperties(rq, ConfigProductCategory.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setType(1)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);
        // 保存产品分类信息
        if (!insert(configProductCategory)) {
            log.error("保存产品分类信息失败，调用{}的{}方法出错", "ConfigProductCategoryServiceImpl", "saveProductCategory()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_SAVE_FAILED);

        }
        return configProductCategory.getId();
    }

    /**
     * 修改产品类别
     *
     * @param userInfo 用户信息
     * @param rq       产品类别信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateProductCategory(AuthPlatformUserInfo userInfo, ConfigProductCategoryUpdateRQ rq) {

        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer id = rq.getId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);

        // 查询默认产品分类id
        List<ConfigProductCategory> defaultCategory = selectList(new EntityWrapper<ConfigProductCategory>()
                .eq("type", 0)
                .eq("deleted", Status.FALSE.getKey()));
        List<Integer> defaultIds = defaultCategory.stream().map(o -> o.getId()).distinct().collect(Collectors.toList());
        List<String> defaultNames = defaultCategory.stream().map(o -> o.getName()).distinct().collect(Collectors.toList());

        // 校验 修改id不为默认数据id(1,2,3)
        if (defaultIds.contains(id)) {
            log.error("id参数异常,不能修改默认产品分类");
            log.error("id参数异常,不能修改默认产品分类,调用{}的{}方法出错", "ConfigProductCategoryServiceImpl", "saveProductCategory()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_UPDATE_FAILED_PROHIBIT_UPDATE_DEFAULT);
        }

        // 校验 分类名称不能与默认的分类名称相同
        if (defaultNames.contains(name)) {
            log.error("修改产品分类信息失败,产品分类名称重复,调用{}的{}方法出错", "ConfigProductCategoryServiceImpl", "saveProductCategory()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_UPDATE_FAILED_NAME_RE);
        }

        // 校验 产品分类 在公司下唯一
        List<ConfigProductCategory> exist = selectList(new EntityWrapper<ConfigProductCategory>()
                .eq("enterprise_id", enterpriseId)
                .ne("id", id)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改产品分类信息失败,设备重复,调用{}的{}方法出错", "ConfigProductCategoryServiceImpl", "updateProductCategory()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_UPDATE_FAILED_NAME_RE);
        }

        // 校验此id的产品分类数据是否存在
        List<ConfigProductCategory> have = selectList(new EntityWrapper<ConfigProductCategory>()
                .eq("enterprise_id", enterpriseId)
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改产品分类失败,此id产品分类不存在,调用{}的{}方法出错", "ConfigDeviceServiceImpl", "updateProductCategory()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_UPDATE_FAILED_NO_DATA);
        }


        ConfigProductCategory configProductCategory = BeanUtils.copyProperties(rq, ConfigProductCategory.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改产品分类信息
        if (!updateById(configProductCategory)) {
            log.error("修改产品分类信息失败，调用{}的{}方法出错", "ConfigProductCategoryServiceImpl", "updateProductCategory()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_UPDATE_FAILED);

        }
        return configProductCategory.getId();
    }

    /**
     * 根据id删除产品类别信息
     *
     * @param userInfo 用户信息
     * @param id       id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteProductCategoryById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();

        // 查询默认产品分类id
        List<ConfigProductCategory> defaultCategory = selectList(new EntityWrapper<ConfigProductCategory>()
                .eq("type", 0)
                .eq("deleted", Status.FALSE.getKey()));
        List<Integer> defaultIds = defaultCategory.stream().map(o -> o.getId()).distinct().collect(Collectors.toList());

        // 校验 修改id不为默认数据id(1,2,3)
        if (defaultIds.contains(id)) {
            log.error("id参数异常,不能删除默认产品分类,id为：" + id);
            log.error("id参数异常,不能删除默认产品分类,调用{}的{}方法出错", "ConfigProductCategoryServiceImpl", "saveProductCategory()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_DELETED_FAILED_PROHIBIT_DELETE_DEFAULT);
        }

        ConfigProductCategory exist = selectOne(new EntityWrapper<ConfigProductCategory>().eq("id", id).eq("deleted", 0));
        // 验证是否存在产品分类信息
        if (ObjectUtils.isEmpty(exist)) {
            log.info("根据id删除产品分类信息，没有找到相应数据，id为：" + id);
            return;
        }
        // 校验设备是否在使用
        if (checkProductCategoryUsed(id)) {
            log.error("删除设备失败,设备已被使用禁止删除,调用{}的{}方法出错", "ConfigDeviceServiceImpl", "updateDevice()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.DEVICE_DELETED_FAILED_USED);
        }

        // 删除设备信息
        if (!updateById(new ConfigProductCategory().setId(id).setDeleted(1).setModifyId(userId).setModifier(userName).setModifyTime(time))) {
            log.error("删除产品分类信息失败，调用{}的{}方法出错", "ConfigProductCategoryServiceImpl", "deleteProductCategoryById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.PRODUCT_CATEGORY_DELETED_FAILED);
        }
    }

    /**
     * 校验产品分类是否在使用
     *
     * @param id id
     * @return 结果
     */
    private boolean checkProductCategoryUsed(Integer id) {
        List<ConfigProduct> products = productService.selectList(new EntityWrapper<ConfigProduct>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("category_id", id));
        if (!CollectionUtils.isEmpty(products)) {
            return true;
        }
        return false;
    }

    /**
     * 获取产品类别列表 下拉使用
     * @param userInfo 用户信息
     * @return 产品类别列表
     */
    @Override
    public List<ConfigProductCategoryDTO> getProductCategoryList(AuthPlatformUserInfo userInfo) {
        Wrapper<ConfigProductCategory> wrapper = new EntityWrapper<ConfigProductCategory>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .eq("status",Status.TRUE.getKey())
                .eq("type",Status.TRUE.getKey())
                .orderBy("create_time", false);

        Wrapper<ConfigProductCategory> wrapper1 = new EntityWrapper<ConfigProductCategory>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status",Status.TRUE.getKey())
                .eq("type",Status.FALSE.getKey())
                .orderBy("create_time", false);

        List<ConfigProductCategory> defaultCategory = baseMapper.selectList(wrapper1);
        List<ConfigProductCategory> productCategoryList = baseMapper.selectList(wrapper);
        if(!CollectionUtils.isEmpty(productCategoryList)){
            defaultCategory.addAll(productCategoryList);
        }

        return BeanUtils.assemble(ConfigProductCategoryDTO.class, defaultCategory);
    }


    @Override
    public ConfigProductCategoryDTO getProductCategoryById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        ConfigProductCategory productCategory = selectOne(new EntityWrapper<ConfigProductCategory>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(productCategory, ConfigProductCategoryDTO.class);
    }
}
