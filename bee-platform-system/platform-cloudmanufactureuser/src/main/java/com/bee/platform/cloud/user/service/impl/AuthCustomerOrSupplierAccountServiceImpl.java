package com.bee.platform.cloud.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.user.dao.mapper.AuthCustomerOrSupplierAccountMapper;
import com.bee.platform.cloud.user.dto.AuthAccountDTO;
import com.bee.platform.cloud.user.dto.AuthCustomerOrSupplierSearchDTO;
import com.bee.platform.cloud.user.dto.CustomerAccountDTO;
import com.bee.platform.cloud.user.entity.*;
import com.bee.platform.cloud.user.rq.*;
import com.bee.platform.cloud.user.service.*;
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

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 客户账号和供应商账号 服务实现类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-18
 */
@Slf4j
@Service
public class AuthCustomerOrSupplierAccountServiceImpl extends ServiceImpl<AuthCustomerOrSupplierAccountMapper, AuthCustomerOrSupplierAccount> implements AuthCustomerOrSupplierAccountService {


    @Autowired
    private AuthPlatformUserService userService;

    @Autowired
    private AuthCustomerOrSupplierService customerOrSupplierService;

    @Autowired
    private AuthEnterpriseService enterpriseService;

    /**
     * 保存客户或供应商账号
     *
     * @param userInfo 用户信息
     * @param rq       请求参数
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer saveAccount(AuthPlatformUserInfo userInfo, AuthAccountSaveRQ rq) {
        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        Integer relatedId = rq.getRelatedId();
        String phone = rq.getPhone();
        Integer type = rq.getType();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        // 校验 手机号 + 类型（0客户 1供应商）在企业下唯一
        List<AuthCustomerOrSupplierAccount> exist = selectList(new EntityWrapper<AuthCustomerOrSupplierAccount>()
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey())
                .eq("type", type)
                .eq("phone", phone));
        if (!CollectionUtils.isEmpty(exist)) {
            log.error("保存客户或供应商账号失败,客户或供应商账号重复,调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "saveAccount()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED_ACCOUNT_RE);
        }

        AuthCustomerOrSupplierAccount account = BeanUtils.copyProperties(rq, AuthCustomerOrSupplierAccount.class)
                .setEnterpriseId(enterpriseId)
                .setFactoryId(factoryId)
                .setCreateId(userId)
                .setCreator(userName)
                .setCreateTime(time)
                .setDeleted(Status.FALSE.getKey());
        // 保存账号信息
        if (!insert(account)) {
            log.error("保存客户或供应商账号失败，调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "saveAccount()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED);
        }

        // 查询用户下有此手机号没  无则新建账号
        List<AuthPlatformUser> haveUser = userService.selectList(new EntityWrapper<AuthPlatformUser>()
                .eq("deleted", Status.FALSE.getKey())
                .eq("username", phone));

        if (!CollectionUtils.isEmpty(haveUser)) {
            // TODO 如果存在账号  不在企业下怎么操作？？
            // 如果有账号 校验账号是否在企业下  若不在关联到企业下


            return account.getId();
        }

       // 用户表无此账号 则新建账号
        AuthCustomerOrSupplier one = customerOrSupplierService.selectById(relatedId);
        if (ObjectUtils.isEmpty(one)) {
            log.error("保存客户或供应商账号失败，没有客户或供应商信息，调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "saveAccount()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED_NO_CS_DATA);
        }
        // 获取客户名称
        String oneName = one.getName();
        Integer carrier = one.getCarrier();

        AuthEnterprise enterprise = enterpriseService.selectOne(new EntityWrapper<AuthEnterprise>()
                .eq("name", oneName)
                .eq("deleted", Status.FALSE.getKey()));

        if (ObjectUtils.isEmpty(enterprise)) {

            log.error("保存客户或供应商账号失败，没有企业信息，调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "saveAccount()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED_NO_E_DATA);

        }
        // 保存用户信息到用户表
        CustomerAccountDTO user = new CustomerAccountDTO().setEnterpriseId(enterprise.getId()).setUsername(phone).setName(name).setCarrier(Status.FALSE.getKey());
        if(Status.TRUE.getKey().equals(carrier)){
            user.setCarrier(Status.TRUE.getKey());
        }
        userService.addCustomerAccount(userInfo,user);


//        else if (type.equals(1)) {
//            AuthCustomerOrSupplier supplier = customerOrSupplierService.selectById(relatedId);
//            if (ObjectUtils.isEmpty(supplier)) {
//                log.error("保存客户或供应商账号失败，调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "saveAccount()");
//                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED);
//            }
//            // 获取客户名称
//            String supplierName = supplier.getName();
//
//            AuthEnterprise enterprise = enterpriseService.selectOne(new EntityWrapper<AuthEnterprise>()
//                    .eq("name", supplierName)
//                    .eq("deleted", Status.FALSE.getKey()));
//
//            if (!ObjectUtils.isEmpty(enterprise)) {
//
//                log.error("保存客户或供应商账号失败，调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "saveAccount()");
//                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED);
//
//            }
//            // 保存用户信息到用户表
//            CustomerAccountDTO user = new CustomerAccountDTO().setEnterpriseId(enterprise.getId()).setUsername(phone).setName(name);
//            userService.addCustomerAccount(userInfo,user);
//
//        }




        return account.getId();
    }

    /**
     * 修改客户或供应商账号
     *
     * @param userInfo 用户信息
     * @param rq       请求参数
     * @return id
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public Integer updateAccount(AuthPlatformUserInfo userInfo, AuthAccountUpdateRQ rq) {

        Date time = new Date();
        Integer userId = userInfo.getId();
        String userName = userInfo.getName();
        Integer enterpriseId = userInfo.getOrgId();
        String name = rq.getName().replaceAll("\\s*", "");
        rq.setName(name);
        Integer id = rq.getId();

        // 校验此id的账号数据是否存在
        List<AuthCustomerOrSupplierAccount> have = selectList(new EntityWrapper<AuthCustomerOrSupplierAccount>()
                .eq("enterprise_id", enterpriseId)
                .eq("id", id)
                .eq("deleted", Status.FALSE.getKey()));
        if (CollectionUtils.isEmpty(have)) {
            log.error("修改账户信息失败,此id数据不存在,调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "updateAccount()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_UPDATE_FAILED_NO_DATA);
        }

        AuthCustomerOrSupplierAccount account = BeanUtils.copyProperties(rq, AuthCustomerOrSupplierAccount.class)
                .setModifyId(userId)
                .setModifier(userName)
                .setModifyTime(time);
        // 修改账号信息
        if (!updateById(account)) {
            log.error("修改账号信息失败，调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "updateAccount()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_UPDATE_FAILED);

        }
        return account.getId();

    }

    /**
     * 条件搜索客户或供应商列表
     *
     * @param userInfo 用户信息
     * @param rq       请求参数
     * @param page     分页对象
     * @return 客户或供应商列表
     */
    @Override
    public ResponseResult<List<AuthCustomerOrSupplierSearchDTO>> searchCustomerOrSupplierList(AuthPlatformUserInfo userInfo, AuthCustomerAndSupplierSearchRQ rq, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);

        Integer enterpriseId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        rq.setEnterpriseId(enterpriseId).setFactoryId(factoryId);

        List<AuthCustomerOrSupplierSearchDTO> dto = baseMapper.searchCustomerOrSupplierList(rq, pagination);
        // 遍历添加 人数字段信息
        dto.forEach(o -> {
            if (o.getType().equals(0)) {
                Integer num = selectCount(new EntityWrapper<AuthCustomerOrSupplierAccount>()
                        .eq("type", 0)
                        .eq("related_id", o.getId())
                        .eq("deleted", 0));
                o.setNum(num);

            }else if(o.getType().equals(1)){
                Integer num = selectCount(new EntityWrapper<AuthCustomerOrSupplierAccount>()
                        .eq("type", 1)
                        .eq("related_id", o.getId())
                        .eq("deleted", 0));
                o.setNum(num);
            }
        });

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));


    }


    /**
     * 条件查询客户或供应商账号列表
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @param page 分页对象
     * @return 账号列表
     */
    @Override
    public ResponseResult<List<AuthAccountDTO>> searchAccountList(AuthPlatformUserInfo userInfo, AuthAccountSearchRQ rq, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<AuthCustomerOrSupplierAccount> wrapper = new EntityWrapper<AuthCustomerOrSupplierAccount>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("deleted", Status.FALSE.getKey())
                .orderBy("id", false);
        wrapper.eq("related_id",rq.getRelatedId()).eq("type",rq.getType());

        if (!StringUtils.isEmpty(rq.getName())) {
            wrapper.like("name", rq.getName());
        }
        if(!ObjectUtils.isEmpty(rq.getPhone())){
            wrapper.eq("phone",rq.getPhone());
        }

        List<AuthCustomerOrSupplierAccount> list = baseMapper.selectPage(pagination, wrapper);
        List<AuthAccountDTO> dto = BeanUtils.assemble(AuthAccountDTO.class, list);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 根据id查询详情
     * @param userInfo 用户信息
     * @param id id
     * @return 账号详情
     */
    @Override
    public AuthAccountDTO getAccountById(AuthPlatformUserInfo userInfo, Integer id) {
        Integer enterpriseId = userInfo.getOrgId();
        AuthCustomerOrSupplierAccount account = selectOne(new EntityWrapper<AuthCustomerOrSupplierAccount>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        return BeanUtils.copyProperties(account, AuthAccountDTO.class);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateUserPassword(AuthPlatformUserInfo userInfo, AuthAccountUpdatePaaswordRQ rq) {
        Integer id = rq.getId();
        Integer enterpriseId = userInfo.getOrgId();
        // 查询 账号信息
        AuthCustomerOrSupplierAccount account = selectOne(new EntityWrapper<AuthCustomerOrSupplierAccount>()
                .eq("id", id)
                .eq("enterprise_id", enterpriseId)
                .eq("deleted", Status.FALSE.getKey()));
        if(ObjectUtils.isEmpty(account)){
            log.error("修改账号密码失败，没有账号信息，调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "updateUserPassword()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_UPDATE_PASSWORD_FAILED_NO_DATA);

        }
        String phone = account.getPhone();
        // 查询用户信息
        AuthPlatformUser user = userService.selectOne(new EntityWrapper<AuthPlatformUser>().eq("deleted", 0).eq("username", phone));

        if(ObjectUtils.isEmpty(user)){
            log.error("修改账号密码失败，没有用户信息，调用{}的{}方法出错", "AuthCustomerOrSupplierAccountServiceImpl", "updateUserPassword()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAILED, ExceptionMessageEnum.CUSTOMER_OR_SUPPLIER_ACCOUNT_UPDATE_PASSWORD_FAILED_NO_USER_DATA);

        }

        UpdatePaaswordRQ updatePaaswordRQ = new UpdatePaaswordRQ().setUserId(user.getId()).setPassword(rq.getPassword());
        // 修改用户密码
        userService.updateUserPassword(userInfo,updatePaaswordRQ);


    }
}
