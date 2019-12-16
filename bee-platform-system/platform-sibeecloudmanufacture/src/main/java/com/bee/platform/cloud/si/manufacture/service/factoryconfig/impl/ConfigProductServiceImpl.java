package com.bee.platform.cloud.si.manufacture.service.factoryconfig.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ConfigProductMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.StorageInventoryService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.*;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyContractBasicService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleContractBasicService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 产品档案 服务实现类
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class ConfigProductServiceImpl extends ServiceImpl<ConfigProductMapper, ConfigProduct> implements ConfigProductService {

    @Autowired
    private ConfigProductTestAttributeInService testAttributeInService;

    @Autowired
    private ConfigProductTestAttributeOutService testAttributeOutService;

    @Autowired
    private ConfigProductSettlementAttributeService settlementAttributeService;

    @Autowired
    private StorageInventoryService storageInventoryService;

    @Autowired
    private BuyContractBasicService buyContractBasicService;

    @Autowired
    private SaleContractBasicService saleContractBasicService;


    @Autowired
    private ConfigProductSpecService productSpecService;

    @Autowired
    private ConfigProductMapper configProductMapper;

    /**
     * 根据条件搜索产品列表
     *
     * @param userInfo 用户信息
     * @param rq       搜索条件
     * @param page     分页对象
     * @return 产品列表
     */
    @Override
    public ResponseResult<List<ConfigProductListDTO>> searchProductList(AuthPlatformUserInfo userInfo, ConfigProductSearchRQ rq, Page page) {
        String name = rq.getName();
        Integer category = rq.getCategoryId();
        Integer status = rq.getStatus();
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ConfigProduct> wrapper = new EntityWrapper<ConfigProduct>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (StringUtils.isNotEmpty(name)) {
            wrapper.like("name", name);
        }
        if (!ObjectUtils.isEmpty(status)) {
            wrapper.eq("status", status);
        }
        if (!ObjectUtils.isEmpty(category)) {
            wrapper.eq("category_id", category);
        }
        List<ConfigProduct> productList = baseMapper.selectPage(pagination, wrapper);
        List<ConfigProductListDTO> dto = BeanUtils.assemble(ConfigProductListDTO.class, productList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 保存产品信息
     *
     * @param userInfo 用户信息
     * @param rq       产品信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveProduct(AuthPlatformUserInfo userInfo, ConfigProductSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        // 校验 产品名称 在公司下唯一
        List<ConfigProduct> exist = selectList(new EntityWrapper<ConfigProduct>()
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name)
        );
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存产品信息失败,产品名称重复,调用{}的{}方法出错", "ConfigProductServiceImpl", "saveProduct()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_SAVE_FAILED_NAME_RE);
        }

        ConfigProduct product = BeanUtils.copyProperties(rq, ConfigProduct.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setDeleted(Status.FALSE.getKey())
                .setCreateTime(time);
        // 保存产品信息
        if (!insert(product)) {
            log.error("保存产品信息失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "saveProduct()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_SAVE_FAILED);

        }
        Integer productId = product.getId();
//        // 保存化验输入项
//        List<ConfigProductTestAttributeInSaveRQ> testAttributeInSaveRQS = rq.getProductTestAttributeInSaveRQS();
//        List<ConfigProductTestAttributeIn> testAttributeIns = BeanUtils.assemble(ConfigProductTestAttributeIn.class, testAttributeInSaveRQS);
//        testAttributeIns.forEach(o -> o.setProductId(productId).setEnterpriseId(enterpriseId).setFactoryId(factoryId)
//                .setCreateId(userId).setCreator(userName).setCreateTime(time));
//        if (!testAttributeInService.insertBatch(testAttributeIns)) {
//            log.error("保存产品化验输入项失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "saveProduct()");
//            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_SAVE_TEST_IN_FAILED);
//
//        }
//
//        // 保存化验结果项
//        List<ConfigProductTestAttributeOutSaveRQ> testAttributeOutSaveRQS = rq.getProductTestAttributeOutSaveRQS();
//        List<ConfigProductTestAttributeOut> testAttributeOuts = BeanUtils.assemble(ConfigProductTestAttributeOut.class, testAttributeOutSaveRQS);
//        testAttributeOuts.forEach(o -> o.setProductId(productId).setEnterpriseId(enterpriseId).setFactoryId(factoryId)
//                .setCreateId(userId).setCreator(userName).setCreateTime(time));
//
//        if (!testAttributeOutService.insertBatch(testAttributeOuts)) {
//            log.error("保存产品化验结果项失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "saveProduct()");
//            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_SAVE_TEST_OUT_FAILED);
//
//        }
        // 保存结算属性
        if(CollectionUtils.isEmpty(rq.getProductSettlementAttributeSaveRQS())){
            log.info("结算属性为空，产品id为："+productId);
            return productId;
        }

        List<ConfigProductSettlementAttributeSaveRQ> settlementAttributeSaveRQS = rq.getProductSettlementAttributeSaveRQS();
        List<ConfigProductSettlementAttribute> settlementAttributes = BeanUtils.assemble(ConfigProductSettlementAttribute.class, settlementAttributeSaveRQS);
        settlementAttributes.forEach(o -> o.setProductId(productId).setEnterpriseId(enterpriseId).setFactoryId(factoryId)
                .setCreateId(userId).setCreator(userName).setCreateTime(time));

        if (!settlementAttributeService.insertBatch(settlementAttributes)) {
            log.error("保存产品结算属性失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "saveProduct()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_SAVE_TEST_OUT_FAILED);

        }

        return productId;
    }

    /**
     * 修改产品信息
     *
     * @param userInfo 用户信息
     * @param rq       产品信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateProduct(AuthPlatformUserInfo userInfo, ConfigProductUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        Integer productId = rq.getId();

        // 校验产品是否正在使用
        if (checkProductUsed(productId)) {
            log.error("修改产品失败,产品正在使用禁止修改,调用{}的{}方法出错", "ConfigProductServiceImpl", "updateProduct()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_UPDATE_FAILED_USED);
        }

        // 校验 产品名称 在公司下唯一
        List<ConfigProduct> exist = selectList(new EntityWrapper<ConfigProduct>()
                .eq("enterprise_id", enterpriseId)
                .ne("id", productId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name)
        );
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("修改产品信息失败,产品名称重复,调用{}的{}方法出错", "ConfigProductServiceImpl", "updateProduct()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_UPDATE_FAILED_NAME_RE);
        }

        // 校验此id的产品数据是否存在
        List<ConfigProduct> have = selectList(new EntityWrapper<ConfigProduct>()
                .eq("enterprise_id", enterpriseId)
                .eq("id", productId)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改产品失败,此id产品数据不存在,调用{}的{}方法出错", "ConfigProductServiceImpl", "updateProduct()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_UPDATE_FAILED_NO_DATA);
        }


        ConfigProduct configProduct = BeanUtils.copyProperties(rq, ConfigProduct.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改产品信息
        if (!updateById(configProduct)) {
            log.error("修改产品信息失败，调用{}的{}方法出错", "a", "updateProduct()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_UPDATE_FAILED);

        }

        // 查询已存在的结算属性旧数据
        Wrapper<ConfigProductSettlementAttribute> wrapper = new EntityWrapper<ConfigProductSettlementAttribute>()
                .eq("product_id", productId)
                .eq("deleted", Status.FALSE.getKey());
        List<ConfigProductSettlementAttribute> old = settlementAttributeService.selectList(wrapper);
        // 存在则删除旧的结算属性
        if (!CollectionUtils.isEmpty(old) && !settlementAttributeService.update(new ConfigProductSettlementAttribute()
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time), wrapper)) {
            log.error("修改产品信息,删除旧的结算属性失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "updateProduct()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_DELETED_SETTLEMENT_ATTRIBUTE_FAILED);
        }

        // 保存结算属性
        if(CollectionUtils.isEmpty(rq.getProductSettlementAttributeSaveRQS())){
            log.info("结算属性为空，产品id为："+productId);
            return productId;
        }

        List<ConfigProductSettlementAttributeSaveRQ> settlementAttributeSaveRQS = rq.getProductSettlementAttributeSaveRQS();
        List<ConfigProductSettlementAttribute> settlementAttributes = BeanUtils.assemble(ConfigProductSettlementAttribute.class, settlementAttributeSaveRQS);
        settlementAttributes.forEach(o -> o.setProductId(productId).setEnterpriseId(enterpriseId).setFactoryId(factoryId)
                .setCreateId(userId).setCreator(userName).setCreateTime(time));

        if (!settlementAttributeService.insertBatch(settlementAttributes)) {
            log.error("保存产品结算属性失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "updateProduct()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_SAVE_TEST_OUT_FAILED);

        }

        return productId;
    }

    /**
     * 校验产品是否正在使用
     *
     * @param productId 产品id
     * @return 结果
     */
    private boolean checkProductUsed(Integer productId) {
        List<StorageInventory> storageInventories = storageInventoryService.selectList(new EntityWrapper<StorageInventory>()
                .eq("status", Status.TRUE.getKey())
                .eq("product_id", productId));
        if (!CollectionUtils.isEmpty(storageInventories)) {
            return true;
        }

        List<BuyContractBasic> buyContractBasics = buyContractBasicService.selectList(new EntityWrapper<BuyContractBasic>()
                .eq("status", Status.TRUE.getKey())
                .eq("product_id", productId));
        if (!CollectionUtils.isEmpty(buyContractBasics)) {
            return true;
        }

        List<SaleContractBasic> saleContractBasics = saleContractBasicService.selectList(new EntityWrapper<SaleContractBasic>()
                .eq("status", Status.TRUE.getKey())
                .eq("product_id", productId));
        if (!CollectionUtils.isEmpty(saleContractBasics)) {
            return true;
        }

        return false;
    }

    /**
     * 根据id删除产品信息
     *
     * @param userInfo 用户信息
     * @param id       id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteProductById(AuthPlatformUserInfo userInfo, Integer id) {

        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();

        // 校验产品是否正在使用
        if (checkProductUsed(id)) {
            log.error("删除产品失败,产品正在使用禁止删除,调用{}的{}方法出错", "ConfigProductServiceImpl", "deleteProductById()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_DELETED_FAILED_USED);
        }

        ConfigProduct exist = selectOne(new EntityWrapper<ConfigProduct>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在产品信息
        if (ObjectUtils.isEmpty(exist)) {
            log.info("根据id删除产品信息，没有找到相应数据，id为：" + id);
            return;
        }
        // 删除产品信息
        if (!updateById(new ConfigProduct()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除产品信息失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "deleteProductById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.PRODUCT_DELETED_FAILED);
        }

        // 删除化验输入项
        // 查询旧的化验输入项
        Wrapper<ConfigProductTestAttributeIn> wrapper1 = new EntityWrapper<ConfigProductTestAttributeIn>()
                .eq("product_id", id)
                .eq("deleted", Status.FALSE.getKey());
        List<ConfigProductTestAttributeIn> old1 = testAttributeInService.selectList(wrapper1);
        // 存在则删除旧的化验输入项
        if (!CollectionUtils.isEmpty(old1) && !testAttributeInService.update(new ConfigProductTestAttributeIn()
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time), wrapper1)) {
            log.error("删除产品信息,删除旧的化验输入项失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "deleteProductById()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_DELETED_TEST_IN_FAILED);
        }
        // 删除化验结果项

        // 查询旧的化验结果项
        Wrapper<ConfigProductTestAttributeOut> wrapper2 = new EntityWrapper<ConfigProductTestAttributeOut>()
                .eq("product_id", id)
                .eq("deleted", Status.FALSE.getKey());
        List<ConfigProductTestAttributeOut> old2 = testAttributeOutService.selectList(wrapper2);
        // 存在则删除旧的化验结果项
        if (!CollectionUtils.isEmpty(old2) && !testAttributeOutService.update(new ConfigProductTestAttributeOut()
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time), wrapper2)) {
            log.error("删除产品信息,删除旧的化验结果项失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "deleteProductById()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_DELETED_TEST_OUT_FAILED);
        }

        // 删除结算属性

        // 查询已存在的结算属性旧数据
        Wrapper<ConfigProductSettlementAttribute> wrapper3 = new EntityWrapper<ConfigProductSettlementAttribute>()
                .eq("product_id", id)
                .eq("deleted", Status.FALSE.getKey());
        List<ConfigProductSettlementAttribute> old3 = settlementAttributeService.selectList(wrapper3);
        // 存在则删除旧的结算属性
        if (!CollectionUtils.isEmpty(old3) && !settlementAttributeService.update(new ConfigProductSettlementAttribute()
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time), wrapper3)) {
            log.error("删除产品信息,删除旧的结算属性失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "deleteProductById()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_DELETED_SETTLEMENT_ATTRIBUTE_FAILED);
        }

    }


    /**
     * 根据id查看产品详情信息
     *
     * @param userInfo 用户信息
     * @param id       id
     * @return 产品详情信息
     */
    @Override
    public ConfigProductDTO getProductById(AuthPlatformUserInfo userInfo, Integer id) {
        // 产品信息
        ConfigProduct product = selectById(id);
        // 化验输入项
        // 查询化验输入项
        Wrapper<ConfigProductTestAttributeIn> wrapper1 = new EntityWrapper<ConfigProductTestAttributeIn>()
                .eq("product_id", id)
                .eq("deleted", Status.FALSE.getKey());
        List<ConfigProductTestAttributeIn> testAttributeIns = testAttributeInService.selectList(wrapper1);
        List<ConfigProductTestAttributeInDTO> testAttributeInDTOS = BeanUtils.assemble(ConfigProductTestAttributeInDTO.class, testAttributeIns);


        // 化验结果项
        // 查询旧的化验输入项
        Wrapper<ConfigProductTestAttributeOut> wrapper2 = new EntityWrapper<ConfigProductTestAttributeOut>()
                .eq("product_id", id)
                .eq("deleted", Status.FALSE.getKey());
        List<ConfigProductTestAttributeOut> testAttributeOuts = testAttributeOutService.selectList(wrapper2);
        List<ConfigProductTestAttributeOutDTO> testAttributeOutDTOS = BeanUtils.assemble(ConfigProductTestAttributeOutDTO.class, testAttributeOuts);


        // 结算属性
        // 查询已存在的结算属性旧数据
        Wrapper<ConfigProductSettlementAttribute> wrapper3 = new EntityWrapper<ConfigProductSettlementAttribute>()
                .eq("product_id", id)
                .eq("deleted", Status.FALSE.getKey());
        List<ConfigProductSettlementAttribute> settlementAttributes = settlementAttributeService.selectList(wrapper3);
        List<ConfigProductSettlementAttributeDTO> settlementAttributeDTOS = BeanUtils.assemble(ConfigProductSettlementAttributeDTO.class, settlementAttributes);

        ConfigProductDTO dto = BeanUtils.copyProperties(product, ConfigProductDTO.class);
        dto.setProductTestAttributeInDTOS(testAttributeInDTOS)
                .setProductTestAttributeOutDTOS(testAttributeOutDTOS)
                .setProductSettlementAttributeDTOS(settlementAttributeDTOS);

        return dto;

    }

    /**
     * 修改产品化验项配置信息
     *
     * @param userInfo 用户信息
     * @param rq       化验项信息
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateTestItem(AuthPlatformUserInfo userInfo, ConfigProductTestItemUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();

        Integer productId = rq.getId();
        // 校验是否存在此产品
        ConfigProduct product = selectOne(new EntityWrapper<ConfigProduct>()
                .eq("id", productId)
                .eq("deleted", Status.FALSE.getKey()));
        if (ObjectUtils.isEmpty(product)) {
            log.error("修改产品化验项配置信息,此id产品数据不存在,调用{}的{}方法出错", "ConfigProductServiceImpl", "updateTestItem()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_UPDATE_TEST_FAILED_NO_DATA);

        }
        // 删除化验输入项
        // 查询旧的化验输入项
        Wrapper<ConfigProductTestAttributeIn> wrapper1 = new EntityWrapper<ConfigProductTestAttributeIn>()
                .eq("product_id", productId)
                .eq("deleted", Status.FALSE.getKey());
        List<ConfigProductTestAttributeIn> old1 = testAttributeInService.selectList(wrapper1);
        // 存在则删除旧的化验输入项
        if (!CollectionUtils.isEmpty(old1) && !testAttributeInService.update(new ConfigProductTestAttributeIn()
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time), wrapper1)) {
            log.error("删除产品信息,删除旧的化验输入项失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "updateTestItem()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_DELETED_TEST_IN_FAILED);
        }
        // 删除化验结果项

        // 查询旧的化验结果项
        Wrapper<ConfigProductTestAttributeOut> wrapper2 = new EntityWrapper<ConfigProductTestAttributeOut>()
                .eq("product_id", productId)
                .eq("deleted", Status.FALSE.getKey());
        List<ConfigProductTestAttributeOut> old2 = testAttributeOutService.selectList(wrapper2);
        // 存在则删除旧的化验结果项
        if (!CollectionUtils.isEmpty(old2) && !testAttributeOutService.update(new ConfigProductTestAttributeOut()
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time), wrapper2)) {
            log.error("删除产品信息,删除旧的化验结果项失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "updateTestItem()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.PRODUCT_DELETED_TEST_OUT_FAILED);
        }


        // 保存化验输入项
        List<ConfigProductTestAttributeInSaveRQ> testAttributeInSaveRQS = rq.getProductTestAttributeInSaveRQS();
        List<ConfigProductTestAttributeIn> testAttributeIns = BeanUtils.assemble(ConfigProductTestAttributeIn.class, testAttributeInSaveRQS);
        testAttributeIns.forEach(o -> o.setProductId(productId).setEnterpriseId(enterpriseId).setFactoryId(factoryId)
                .setCreateId(userId).setCreator(userName).setCreateTime(time));
        if (!testAttributeInService.insertBatch(testAttributeIns)) {
            log.error("保存产品化验输入项失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "updateTestItem()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_SAVE_TEST_IN_FAILED);

        }

        // 保存化验结果项
        List<ConfigProductTestAttributeOutSaveRQ> testAttributeOutSaveRQS = rq.getProductTestAttributeOutSaveRQS();
        List<ConfigProductTestAttributeOut> testAttributeOuts = BeanUtils.assemble(ConfigProductTestAttributeOut.class, testAttributeOutSaveRQS);
        testAttributeOuts.forEach(o -> o.setProductId(productId).setEnterpriseId(enterpriseId).setFactoryId(factoryId)
                .setCreateId(userId).setCreator(userName).setCreateTime(time));

        if (!testAttributeOutService.insertBatch(testAttributeOuts)) {
            log.error("保存产品化验结果项失败，调用{}的{}方法出错", "ConfigProductServiceImpl", "updateTestItem()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.PRODUCT_SAVE_TEST_OUT_FAILED);

        }

        return productId;
    }

    /**
     * 根据产品id查询产品输入检测项
     */
    @Override
    public List<ConfigProductTestAttributeInDTO> getProductAttributeInByProductId(Integer productId, AuthPlatformUserInfo userInfo) {
        List<ConfigProductTestAttributeIn> testAttributeInList = testAttributeInService
                .selectList(new EntityWrapper<>(new ConfigProductTestAttributeIn()
                        .setProductId(productId)
                        .setDeleted(Status.FALSE.getKey())
                        .setEnterpriseId(userInfo.getOrgId())));
        List<ConfigProductTestAttributeInDTO> dtoList = BeanUtils.assemble(ConfigProductTestAttributeInDTO.class, testAttributeInList);
        dtoList.forEach(a -> {
            if (a.getTestUnit() != null && a.getTestUnit().equals(0)) {
                a.setUnitString(EnumSampleRelation.ProductUnit.PERCENTAGE.getValue());
            } else if (a.getTestUnit() != null && a.getTestUnit().equals(1)) {
                a.setUnitString(EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue());
            }
        });
        return dtoList;
    }

    /**
     * 根据产品id查询产品输出检测项
     */
    @Override
    public List<ConfigProductTestAttributeOutDTO> getProductAttributeOutByProductId(Integer productId, AuthPlatformUserInfo userInfo) {
        List<ConfigProductTestAttributeOut> testAttributeOutList = testAttributeOutService
                .selectList(new EntityWrapper<>(new ConfigProductTestAttributeOut()
                        .setProductId(productId)
                        .setEnterpriseId(userInfo.getOrgId())
                        .setDeleted(Status.FALSE.getKey()))
                );
        List<ConfigProductTestAttributeOutDTO> dtoList = BeanUtils.assemble(ConfigProductTestAttributeOutDTO.class, testAttributeOutList);
        dtoList.forEach(a -> {
            if (a.getTestUnit() != null && a.getTestUnit().equals(0)) {
                a.setUnitString(EnumSampleRelation.ProductUnit.PERCENTAGE.getValue());
            } else if (a.getTestUnit() != null && a.getTestUnit().equals(1)) {
                a.setUnitString(EnumSampleRelation.ProductUnit.EXTREME_RATIO.getValue());
            }
        });
        return dtoList;
    }

    /**
     * 根据产品类型查询产品列表
     *
     * @param userInfo 用户信息
     * @param category 产品类别 0 全部 1 主料 2 辅料 3 成品 4 用户定义的类别  5 包含(主料 辅料)  6 包含(主料  成品) 7包含(辅料  成品) 8包含(主料 辅料 成品)
     * @return 产品列表
     */
    @Override
    public List<ConfigProductListDTO> getProductListByCategory(AuthPlatformUserInfo userInfo, Integer category) {
        Wrapper<ConfigProduct> wrapper = new EntityWrapper<ConfigProduct>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status", Status.TRUE.getKey())
                .orderBy("id", false)
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId());

        switch (category) {
            case 1:
                wrapper.eq("category_id", 1);
                break;
            case 2:
                wrapper.eq("category_id", 2);
                break;
            case 3:
                wrapper.eq("category_id", 3);
                break;
            case 4:
                wrapper.notIn("category_id", Lists.newArrayList(1, 2, 3));
                break;
            case 5:
                wrapper.in("category_id", Lists.newArrayList(1, 2));
                break;
            case 6:
                wrapper.in("category_id", Lists.newArrayList(1, 3));
                break;
            case 7:
                wrapper.in("category_id", Lists.newArrayList(2, 3));
                break;
            case 8:
                wrapper.in("category_id", Lists.newArrayList(1, 2, 3));
                break;
            default:
                break;
        }

        List<ConfigProduct> products = selectList(wrapper);


        Wrapper<ConfigProductSpec> wrapper2 = new EntityWrapper<ConfigProductSpec>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status", Status.TRUE.getKey())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId());

        List<ConfigProductSpec> productSpecs = productSpecService.selectList(wrapper2);
        List<Integer> productId = productSpecs.stream().map(o -> o.getProductId()).distinct().collect(Collectors.toList());


        List<ConfigProduct> dto = products.stream().filter(o -> productId.contains(o.getId())).collect(Collectors.toList());

        return BeanUtils.assemble(ConfigProductListDTO.class, dto);
    }


    /**
     * @Description 判断产品是否拥有化验项
     * @author chenxm66777123
     * @Date 2019/10/25 16:34
     * @version 1.0.0
     */
    @Override
    public boolean judgeHaveProductAttribute(Integer productId, AuthPlatformUserInfo userInfo) {
        List<ConfigProductTestAttributeIn> testAttributeInList = testAttributeInService
                .selectList(new EntityWrapper<>(new ConfigProductTestAttributeIn()
                        .setProductId(productId)
                        .setDeleted(Status.FALSE.getKey())
                        .setEnterpriseId(userInfo.getOrgId())));
        if(ObjectUtils.isEmpty(testAttributeInList)){
            return false;
        }

        List<ConfigProductTestAttributeOut> testAttributeOutList = testAttributeOutService
                .selectList(new EntityWrapper<>(new ConfigProductTestAttributeOut()
                        .setProductId(productId)
                        .setEnterpriseId(userInfo.getOrgId())
                        .setDeleted(Status.FALSE.getKey()))
                );
        if(ObjectUtils.isEmpty(testAttributeOutList)){
            return false;
        }

        return true;
    }
    /**
     * @notes: 查询当前工厂所有的产品类别 ，暂时只返回产品id 和产品名称，如有需要可自行添加
     * @Author: junyang.li
     * @Date: 10:24 2019/11/26
     * @param userInfo : 当前操作人
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.ConfigProduct>
     */
    @Override
    public List<ConfigProduct> getProductList(AuthPlatformUserInfo userInfo) {
        Integer factoryId=userInfo.getFactoryId();
        if(factoryId==null){
            return new ArrayList<>();
        }
        return configProductMapper.getProductList(userInfo.getFactoryId());
    }
}
