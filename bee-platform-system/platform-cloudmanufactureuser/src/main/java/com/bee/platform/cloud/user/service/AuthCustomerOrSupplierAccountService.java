package com.bee.platform.cloud.user.service;

import com.bee.platform.cloud.user.dto.AuthAccountDTO;
import com.bee.platform.cloud.user.dto.AuthCustomerOrSupplierSearchDTO;
import com.bee.platform.cloud.user.entity.AuthCustomerOrSupplierAccount;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.user.rq.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 客户账号和供应商账号 服务类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-18
 */
public interface AuthCustomerOrSupplierAccountService extends IService<AuthCustomerOrSupplierAccount> {
    /**
     * 保存客户或供应商账号
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer saveAccount(AuthPlatformUserInfo userInfo, AuthAccountSaveRQ rq);

    /**
     * 修改客户或供应商账号
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @return id
     */
    Integer updateAccount(AuthPlatformUserInfo userInfo, AuthAccountUpdateRQ rq);

    /**
     * 条件搜索客户或供应商列表
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @param page 分页对象
     * @return 客户或供应商列表
     */
    ResponseResult<List<AuthCustomerOrSupplierSearchDTO>> searchCustomerOrSupplierList(AuthPlatformUserInfo userInfo, AuthCustomerAndSupplierSearchRQ rq, Page page);


    /**
     * 条件查询客户或供应商账号列表
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @param page 分页对象
     * @return 账号列表
     */
    ResponseResult<List<AuthAccountDTO>> searchAccountList(AuthPlatformUserInfo userInfo, AuthAccountSearchRQ rq, Page page);

    /**
     * 根据id查询详情
     * @param userInfo 用户信息
     * @param id id
     * @return 账号详情
     */
    AuthAccountDTO getAccountById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 修改用户密码
     *
     * @param userInfo 用户信息
     * @param rq 请求参数
     */
    void updateUserPassword(AuthPlatformUserInfo userInfo, AuthAccountUpdatePaaswordRQ rq);
}
