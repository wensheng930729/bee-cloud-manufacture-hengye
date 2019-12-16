package com.bee.platform.cloud.user.controller;


import com.bee.platform.cloud.user.dto.AuthAccountDTO;
import com.bee.platform.cloud.user.dto.AuthCustomerOrSupplierSearchDTO;
import com.bee.platform.cloud.user.rq.*;
import com.bee.platform.cloud.user.service.AuthCustomerOrSupplierAccountService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 客户账号和供应商账号 前端控制器
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-18
 */

@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "authCustomerOrSupplierAccount", tags = "C-客户账号和供应商账号相关接口")
@RestController
@RequestMapping("/authCustomerOrSupplierAccount")
public class AuthCustomerOrSupplierAccountController {

    @Autowired
    private AuthCustomerOrSupplierAccountService accountService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @ApiOperation(value = "保存客户账号或供应商账号信息")
    @PostMapping("/saveAccount")
    public ResponseResult<Integer> saveAccount(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid AuthAccountSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = accountService.saveAccount(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改客户账号或供应商账号信息")
    @PostMapping("/updateAccount")
    public ResponseResult<Integer> updateAccount(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid AuthAccountUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = accountService.updateAccount(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }



    @PostMapping("/searchCustomerOrSupplierList")
    @ApiOperation(value = "条件查询客户或供应商列表")
    public ResponseResult<List<AuthCustomerOrSupplierSearchDTO>> searchCustomerOrSupplierList(@RequestHeader("sysToken") String sysToken, @RequestBody AuthCustomerAndSupplierSearchRQ rq, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return accountService.searchCustomerOrSupplierList(userInfo,rq,page);
    }



    @PostMapping("/searchAccountList")
    @ApiOperation(value = "条件查询客户或供应商账号列表")
    public ResponseResult<List<AuthAccountDTO>> searchAccountList(@RequestHeader("sysToken") String sysToken, @RequestBody AuthAccountSearchRQ rq, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return accountService.searchAccountList(userInfo,rq,page);
    }



    @GetMapping("/getAccountById")
    @ApiOperation(value = "根据id查询账号详情")
    public ResponseResult<AuthAccountDTO> getAccountById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        AuthAccountDTO dto = accountService.getAccountById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }



    @PostMapping("/updateUserPassword")
    @ApiOperation(value = "工厂配置 - 上下游账号 - 修改用户密码")
    public ResponseResult<ResCodeEnum> updatePassword(@RequestHeader("sysToken")String sysToken,
                                                          @RequestBody @Valid AuthAccountUpdatePaaswordRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        //修改用户密码
         accountService.updateUserPassword(userInfo,rq);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

}

