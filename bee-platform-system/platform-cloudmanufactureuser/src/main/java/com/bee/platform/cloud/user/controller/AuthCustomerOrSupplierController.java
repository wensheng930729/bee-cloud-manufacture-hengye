package com.bee.platform.cloud.user.controller;


import com.bee.platform.cloud.user.dto.AuthCustomerAndSupplierDTO;
import com.bee.platform.cloud.user.service.AuthCustomerOrSupplierService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * 供应商或客户管理 前端控制器
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-21
 */

@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "authCustomerOrSupplier", tags = "C-供应商或客户管理相关接口")
@RestController
@RequestMapping("/authCustomerOrSupplier")
public class AuthCustomerOrSupplierController {

    @Autowired
    private AuthCustomerOrSupplierService customerOrSupplierService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/getAllCustomerAndSupplier")
    @ApiOperation(value = "查询用户公司下的客户和供应商列表")
    public ResponseResult<List<AuthCustomerAndSupplierDTO>> getAllCustomerAndSupplier(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        List<AuthCustomerAndSupplierDTO> dto =  customerOrSupplierService.getAllCustomerAndSupplier(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}

