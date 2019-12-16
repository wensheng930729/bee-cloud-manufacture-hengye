package com.bee.platform.cloud.user.service;

import com.bee.platform.cloud.user.dto.*;
import com.bee.platform.cloud.user.entity.AuthCustomerOrSupplier;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.user.rq.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 供应商管理 服务类
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-21
 */
public interface AuthCustomerOrSupplierService extends IService<AuthCustomerOrSupplier> {

//  客户相关

    ResponseResult<List<AuthCustomerDTO>> searchCustomerList(AuthPlatformUserInfo userInfo, AuthCustomerSearchRQ rq, Page page);

    Integer saveCustomer(AuthPlatformUserInfo userInfo, AuthCustomerSaveRQ rq);

    Integer updateCustomer(AuthPlatformUserInfo userInfo, AuthCustomerUpdateRQ rq);

    void deleteCustomerById(AuthPlatformUserInfo userInfo, Integer id);

    AuthCustomerDTO getCustomerById(AuthPlatformUserInfo userInfo, Integer id);

    List<AuthCustomerDTO> getCustomerListByType(AuthPlatformUserInfo userInfo, List<Integer> types);

    AuthCustomerNameDTO getCustomerNameById(Integer id);




// 供应商相关

    ResponseResult<List<AuthSupplierDTO>> searchSupplierList(AuthPlatformUserInfo userInfo, AuthSupplierSearchRQ rq, Page page);

    Integer saveSupplier(AuthPlatformUserInfo userInfo, AuthSupplierSaveRQ rq);

    Integer updateSupplier(AuthPlatformUserInfo userInfo, AuthSupplierUpdateRQ rq);

    void deleteSupplierById(AuthPlatformUserInfo userInfo, Integer id);

    /**
     * 查询用户所属公司的承运商信息
     * @param userInfo
     * @return
     */
    ResponseResult<List<CarrierInfoDTO>> getCarrierInfoList(AuthPlatformUserInfo userInfo);

    AuthSupplierDTO getSupplierById(AuthPlatformUserInfo userInfo, Integer id);

    List<AuthSupplierDTO> getSupplierListByCategory(AuthPlatformUserInfo userInfo, List<Integer> types);

    AuthSupplierNameDTO getSupplierNameById(Integer id);

    List<AuthCustomerAndSupplierDTO> getAllCustomerAndSupplier(AuthPlatformUserInfo userInfo);
}
