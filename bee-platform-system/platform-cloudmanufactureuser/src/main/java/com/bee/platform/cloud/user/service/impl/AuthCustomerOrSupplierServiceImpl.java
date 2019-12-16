package com.bee.platform.cloud.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.user.dao.mapper.AuthCustomerOrSupplierMapper;
import com.bee.platform.cloud.user.dto.*;
import com.bee.platform.cloud.user.entity.AuthCustomerOrSupplier;
import com.bee.platform.cloud.user.entity.AuthEnterprise;
import com.bee.platform.cloud.user.rq.*;
import com.bee.platform.cloud.user.service.AuthCustomerOrSupplierService;
import com.bee.platform.cloud.user.service.AuthEnterpriseService;
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
import org.springframework.util.StringUtils;

import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 供应商管理 服务实现类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-21
 */
@Slf4j
@Service
public class AuthCustomerOrSupplierServiceImpl extends ServiceImpl<AuthCustomerOrSupplierMapper, AuthCustomerOrSupplier> implements AuthCustomerOrSupplierService {


    @Autowired
    private AuthEnterpriseService enterpriseService;

    // 客户相关--------------------------------------------------------------------

    @Override
    public ResponseResult<List<AuthCustomerDTO>> searchCustomerList(AuthPlatformUserInfo userInfo, AuthCustomerSearchRQ rq, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<AuthCustomerOrSupplier> wrapper = new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("type",0)
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (!StringUtils.isEmpty(rq.getName())) {
            wrapper.like("name", rq.getName());
        }
        if(!ObjectUtils.isEmpty(rq.getCategory())){
            wrapper.eq("category",rq.getCategory());
        }
        if(!ObjectUtils.isEmpty(rq.getStatus())){
            wrapper.eq("status",rq.getStatus());
        }
        List<AuthCustomerOrSupplier> customerList = baseMapper.selectPage(pagination, wrapper);
        List<AuthCustomerDTO> dto = BeanUtils.assemble(AuthCustomerDTO.class, customerList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveCustomer(AuthPlatformUserInfo userInfo, AuthCustomerSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        // 校验客户名称在公司下唯一
        List<AuthCustomerOrSupplier> exist = selectList(new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("enterprise_id", enterpriseId)
                .eq("type",0)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存客户信息失败,客户名称重复,调用{}的{}方法出错", "ConfigCustomerServiceImpl", "saveCustomer()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_SAVE_FAILED_NAME_RE);
        }

        AuthCustomerOrSupplier customer = BeanUtils.copyProperties(rq, AuthCustomerOrSupplier.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setType(0)
                .setCreateId(userId)
                .setCreator(userName)
                .setCreateTime(time)
                .setDeleted(Status.FALSE.getKey());
        // 保存客户信息
        if (!insert(customer)) {
            log.error("保存客户信息失败，调用{}的{}方法出错", "ConfigCustomerServiceImpl", "saveCustomer()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_SAVE_FAILED);
        }

        // 校验客户--企业名称是否在平台有数据
        AuthEnterprise enterprise = enterpriseService.selectOne(new EntityWrapper<AuthEnterprise>()
                .eq("name", name)
                .eq("deleted", Status.FALSE.getKey()));

        if(!ObjectUtils.isEmpty(enterprise)){

            return customer.getId();
        }

        AuthEnterprise authEnterprise = new AuthEnterprise()
                .setName(name)
                .setType(Status.TRUE.getKey())
                .setOperateId(userId)
                .setCreateTime(time)
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey());

        // 没有数据生成企业信息
        if(!enterpriseService.insert(authEnterprise)){
            log.error("保存客户信息，生成企业信息失败，调用{}的{}方法出错", "ConfigCustomerServiceImpl", "saveCustomer()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_CREAT_ENTERPRISE_SAVE_FAILED);
        }

        return customer.getId();
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateCustomer(AuthPlatformUserInfo userInfo, AuthCustomerUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer id = rq.getId();
        // 校验此id的客户数据是否存在
        List<AuthCustomerOrSupplier> have = selectList(new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("enterprise_id", enterpriseId)
                .eq("id",id)
                .eq("type",0)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改客户信息失败,此id客户不存在,调用{}的{}方法出错", "ConfigCustomerServiceImpl", "updateCustomer()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.CUSTOMER_UPDATE_FAILED_NO_DATA);
        }

        AuthCustomerOrSupplier customer = BeanUtils.copyProperties(rq, AuthCustomerOrSupplier.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改客户信息
        if (!updateById(customer)) {
            log.error("修改客户信息失败，调用{}的{}方法出错", "ConfigCustomerServiceImpl", "updateCustomer()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.CUSTOMER_UPDATE_FAILED);

        }
        return customer.getId();
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteCustomerById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        AuthCustomerOrSupplier exist = selectOne(new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在客户信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除客户信息，没有找到相应数据，id为："+id);
            return;
        }
        // 删除客户信息
        if (!updateById(new AuthCustomerOrSupplier()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除客户信息失败，调用{}的{}方法出错", "ConfigCustomerServiceImpl", "deleteCustomerById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.CUSTOMER_DELETED_FAILED);
        }
    }


    @Override
    public AuthCustomerDTO getCustomerById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        AuthCustomerOrSupplier customer = selectOne(new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("id", id)
                .eq("type",0)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(customer, AuthCustomerDTO.class);
    }


    @Override
    public List<AuthCustomerDTO> getCustomerListByType(AuthPlatformUserInfo userInfo, List<Integer> types) {
        Wrapper<AuthCustomerOrSupplier> wrapper = new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status",Status.TRUE.getKey())
                .eq("type",0)
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId());
        if(!CollectionUtils.isEmpty(types)){
            wrapper.in("category",types);
        }

        List<AuthCustomerOrSupplier> customers = selectList(wrapper);

        return BeanUtils.assemble(AuthCustomerDTO.class,customers);
    }


    @Override
    public AuthCustomerNameDTO getCustomerNameById(Integer id) {

        AuthCustomerOrSupplier customer = selectById(id);

        return BeanUtils.copyProperties(customer,AuthCustomerNameDTO.class);
    }





    // 供应商相关--------------------------------------------------------------------

    @Override
    public ResponseResult<List<AuthSupplierDTO>> searchSupplierList(AuthPlatformUserInfo userInfo, AuthSupplierSearchRQ rq, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<AuthCustomerOrSupplier> wrapper = new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("type",1)
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        if (!StringUtils.isEmpty(rq.getName())) {
            wrapper.like("name", rq.getName());
        }
        if(!ObjectUtils.isEmpty(rq.getCategory())){
            wrapper.eq("category",rq.getCategory());
        }
        if(!ObjectUtils.isEmpty(rq.getStatus())){
            wrapper.eq("status",rq.getStatus());
        }
        List<AuthCustomerOrSupplier> supplierList = baseMapper.selectPage(pagination, wrapper);
        List<AuthSupplierDTO> dto = BeanUtils.assemble(AuthSupplierDTO.class, supplierList);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveSupplier(AuthPlatformUserInfo userInfo, AuthSupplierSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        Integer carrier = rq.getCarrier();
        Integer carrierCategory = rq.getCarrierCategory();

        // 如果不是承运商 则承运商类别设为null
        if(Status.FALSE.getKey().equals(carrier) ){
            rq.setCarrierCategory(null);
        }
        if(Status.TRUE.getKey().equals(carrier) && ObjectUtils.isEmpty(carrierCategory)){
            log.error("供应商为承运商时，承运商类别不能为空");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.SUPPLIER_SAVE_FAILED_CARRIER_C_EMPTY);
        }

        // 校验供应商名称在公司下唯一
        List<AuthCustomerOrSupplier> exist = selectList(new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("enterprise_id", enterpriseId)
                .eq("type",1)
                .eq("deleted", Status.FALSE.getKey())
                .eq("name", name));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存供应商信息失败,供应商名称重复,调用{}的{}方法出错", "ConfigSupplierServiceImpl", "saveSupplier()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.SUPPLIER_SAVE_FAILED_NAME_RE);
        }

        AuthCustomerOrSupplier supplier = BeanUtils.copyProperties(rq, AuthCustomerOrSupplier.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setType(1)
                .setCreateId(userId)
                .setCreator(userName)
                .setCreateTime(time)
                .setDeleted(Status.FALSE.getKey());
        // 保存供应商信息
        if (!insert(supplier)) {
            log.error("保存供应商信息失败，调用{}的{}方法出错", "ConfigSupplierServiceImpl", "saveSupplier()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.SUPPLIER_SAVE_FAILED);
        }

        // 校验供应商--企业名称是否在平台有数据
        AuthEnterprise enterprise = enterpriseService.selectOne(new EntityWrapper<AuthEnterprise>()
                .eq("name", name)
                .eq("deleted", Status.FALSE.getKey()));

        if(!ObjectUtils.isEmpty(enterprise)){

            return supplier.getId();
        }

        AuthEnterprise authEnterprise = new AuthEnterprise()
                .setName(name)
                .setType(Status.TRUE.getKey())
                .setOperateId(userId)
                .setCreateTime(time)
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey());

        // 没有数据生成企业信息
        if(!enterpriseService.insert(authEnterprise)){
            log.error("保存供应商信息，生成企业信息失败，调用{}的{}方法出错", "ConfigSupplierServiceImpl", "saveSupplier()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.SUPPLIER_CREAT_ENTERPRISE_SAVE_FAILED);
        }

        return supplier.getId();
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateSupplier(AuthPlatformUserInfo userInfo, AuthSupplierUpdateRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer id = rq.getId();
        Integer carrier = rq.getCarrier();
        Integer carrierCategory = rq.getCarrierCategory();
        // 如果不是承运商 则承运商类别设为null
        if(Status.FALSE.getKey().equals(carrier) ){
            rq.setCarrierCategory(null);
        }
        if(Status.TRUE.getKey().equals(carrier) && ObjectUtils.isEmpty(carrierCategory)){
            log.error("承运商类别不能为空");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.SUPPLIER_UPDATE_FAILED_CARRIER_C_EMPTY);
        }


        // 校验此id的供应商数据是否存在
        List<AuthCustomerOrSupplier> have = selectList(new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("enterprise_id", enterpriseId)
                .eq("id",id)
                .eq("type",1)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改供应商信息失败,此id供应商不存在,调用{}的{}方法出错", "ConfigSupplierServiceImpl", "updateSupplier()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.SUPPLIER_UPDATE_FAILED_NO_DATA);
        }

        AuthCustomerOrSupplier supplier = BeanUtils.copyProperties(rq, AuthCustomerOrSupplier.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改供应商信息
        if (!updateById(supplier)) {
            log.error("修改供应商信息失败，调用{}的{}方法出错", "ConfigSupplierServiceImpl", "updateSupplier()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.SUPPLIER_UPDATE_FAILED);

        }
        return supplier.getId();
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void deleteSupplierById(AuthPlatformUserInfo userInfo, Integer id) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        AuthCustomerOrSupplier exist = selectOne(new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        // 验证是否存在供应商信息
        if(ObjectUtils.isEmpty(exist)){
            log.info("根据id删除供应商信息，没有找到相应数据，id为："+id);
            return;
        }
        // 删除供应商信息
        if (!updateById(new AuthCustomerOrSupplier()
                .setId(id)
                .setDeleted(Status.TRUE.getKey())
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time))) {
            log.error("删除供应商信息失败，调用{}的{}方法出错", "ConfigSupplierServiceImpl", "deleteSupplierById()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.SUPPLIER_DELETED_FAILED);
        }
    }


    @Override
    public AuthSupplierDTO getSupplierById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        AuthCustomerOrSupplier supplier = selectOne(new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("id", id)
                .eq("type",1)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(supplier,AuthSupplierDTO.class);
    }

    /**
     * 查询用户所属公司的承运商信息
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<List<CarrierInfoDTO>> getCarrierInfoList(AuthPlatformUserInfo userInfo) {

        List<CarrierInfoDTO> carrierInfoDTOS = new ArrayList<>();

        //查询用户所属公司相关的承运商信息
        List<AuthCustomerOrSupplier> configSuppliers = this.selectList(new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("carrier", 1)
                .eq("type",1)
                .eq("deleted", Status.FALSE.getKey())
                .eq("status", Status.TRUE.getKey()));

        if (!CollectionUtils.isEmpty(configSuppliers)) {
            //不为空则提取数据
            configSuppliers.forEach(supplier -> carrierInfoDTOS.add(new CarrierInfoDTO()
                    .setCarrierId(supplier.getId().longValue()).setCarrierName(supplier.getName())));
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, carrierInfoDTOS);
    }


    @Override
    public List<AuthSupplierDTO> getSupplierListByCategory(AuthPlatformUserInfo userInfo, List<Integer> types) {
        Wrapper<AuthCustomerOrSupplier> wrapper = new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status",Status.TRUE.getKey())
                .eq("type",1)
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId());
        if(!CollectionUtils.isEmpty(types)){
            wrapper.in("category",types);
        }

        List<AuthCustomerOrSupplier> suppliers = selectList(wrapper);

        return BeanUtils.assemble(AuthSupplierDTO.class,suppliers);
    }

    @Override
    public AuthSupplierNameDTO getSupplierNameById(Integer id) {
        AuthCustomerOrSupplier supplier = selectById(id);

        return BeanUtils.copyProperties(supplier,AuthSupplierNameDTO.class);
    }




    @Override
    public List<AuthCustomerAndSupplierDTO> getAllCustomerAndSupplier(AuthPlatformUserInfo userInfo) {
        Wrapper<AuthCustomerOrSupplier> wrapper = new EntityWrapper<AuthCustomerOrSupplier>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("status",Status.TRUE.getKey())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("enterprise_id", userInfo.getOrgId())
                .groupBy("name");

        List<AuthCustomerOrSupplier> allCustomerOrSupplier = selectList(wrapper);

        return BeanUtils.assemble(AuthCustomerAndSupplierDTO.class, allCustomerOrSupplier);
    }
}
