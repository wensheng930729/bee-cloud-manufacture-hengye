package com.bee.platform.cloud.user.controller;

import com.bee.platform.cloud.user.rq.AuthUserRQ;
import com.bee.platform.cloud.user.rq.CodeLoginRQ;
import com.bee.platform.cloud.user.service.AuthUserLoginService;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.constants.enums.PlatformType;
import com.bee.platform.common.entity.AppUserInfo;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.WebUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

/**
 * @Description 登录
 * @Date 2019/5/24 10:17
 * @Author xin.huang
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/authPlatformUser")
@Api(value = "新权限：用户登录接口", tags = "新权限：用户登录接口")
public class AuthUserLoginController {

    @Autowired
    private AuthUserLoginService authUserLoginService;

    @NotIntercept
    @ApiOperation(value = "用户web端 - 账号密码登录", notes = "用户登录")
    @RequestMapping(value = "/web/login", method = RequestMethod.POST)
    public ResponseResult<AuthPlatformUserInfo> login(@RequestBody @Valid AuthUserRQ authUserRQ,HttpServletRequest request){
        if(StringUtils.isEmpty(authUserRQ.getUsername())||StringUtils.isEmpty(authUserRQ.getPassword())){
            return ResponseResult.buildResponseResult(ResCodeEnum.USERNAME_OR_PASSWORD_ERROR);
        }
        if(StringUtils.isEmpty(authUserRQ.getCurrentClientId())){
            authUserRQ.setCurrentClientId(WebUtils.getIpAddress(request));
        }
        return authUserLoginService.login(authUserRQ,PlatformType.CLOUD_MAF_WEB);
    }

    @NotIntercept
    @ApiOperation(value = "用户app端 - 账号密码登录", notes = "用户登录")
    @RequestMapping(value = "/client/login", method = RequestMethod.POST)
    public ResponseResult<AppUserInfo> clientLogin(@RequestBody @Valid AuthUserRQ authUserRQ,HttpServletRequest request){
        if(StringUtils.isEmpty(authUserRQ.getCurrentClientId())){
            authUserRQ.setCurrentClientId(WebUtils.getIpAddress(request));
        }
        return authUserLoginService.clientLogin(authUserRQ);
    }

    @NotIntercept
    @ApiOperation(value = "用户app端 - 验证码登录", notes = "用户登录")
    @RequestMapping(value = "/client/code/login", method = RequestMethod.POST)
    public ResponseResult<AppUserInfo> appCodeLogin(@RequestBody @Valid CodeLoginRQ rq,HttpServletRequest request){
        if(StringUtils.isEmpty(rq.getCurrentClientId())){
            rq.setCurrentClientId(WebUtils.getIpAddress(request));
        }
        log.info("本次登录的账号和验证码是:{}",rq);
        return authUserLoginService.appCodeLogin(rq);
    }

    @NotIntercept
    @ApiOperation(value = "用户web端 - 验证码登录", notes = "用户登录")
    @RequestMapping(value = "/web/code/login", method = RequestMethod.POST)
    public ResponseResult<AuthPlatformUserInfo> codeLogin(@RequestBody @Valid CodeLoginRQ rq
                                                            ,HttpServletRequest request){
        log.info("本次登录的账号和验证码是:{}",rq);
        if(StringUtils.isEmpty(rq.getCurrentClientId())){
            rq.setCurrentClientId(WebUtils.getIpAddress(request));
        }
        return authUserLoginService.codeLogin(rq);
    }

    @NotIntercept
    @ApiOperation(value = "注销登录")
    @RequestMapping(value = "/logout", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> logout(@RequestHeader("sysToken")String sysToken) {
        if(StringUtils.isEmpty(sysToken)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        try {
            return authUserLoginService.logout(sysToken);
        }catch (Exception e){
            log.error("注销登录系统异常，异常信息是：{}",e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
