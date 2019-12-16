package com.bee.platform.cloud.user.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.user.dto.UserDetailedDTO;
import com.bee.platform.cloud.user.rq.AddUserRQ;
import com.bee.platform.cloud.user.rq.EditPasswordRQ;
import com.bee.platform.cloud.user.rq.EditUserRQ;
import com.bee.platform.cloud.user.rq.UserIdsRQ;
import com.bee.platform.cloud.user.service.AuthPlatformUserService;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.Validator;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
@Slf4j
@RestController
@RequestMapping("/platform/auth")
@CrossOrigin(origins = "*")
@Api(value = "新权限：用户操作相关接口", tags = "新权限：用户操作相关接口")
public class AuthPlatformUserController {

    @Autowired
    private AuthPlatformUserService authPlatformUserService;

    @NotIntercept
    @ApiOperation(value = "用于存在的用户请求验证码", notes = "用户找回密码请求验证码、企业注册绑定管理员请求验证码、转让超级管理员")
    @ApiImplicitParam(name = "phone", value = "手机号", required = true)
    @RequestMapping(value = "/getValidateCodeWithHasAccount", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> requestValidateCodeWithHasAccount(String phone) {
        if (!Validator.isMobile(phone)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        return authPlatformUserService.sendMessage(phone);
    }


    @ApiOperation(value = "登陆后更新密码")
    @RequestMapping(value = "/updatePassword", method = RequestMethod.POST)
    public ResponseResult<ResCodeEnum> updatePassword(EditPasswordRQ rq) {
        if(!Validator.isPassword(rq.getNewPassword())){
            return  ResponseResult.buildResponseResult(ResCodeEnum.PASSWORD_ERROR);
        }
        return authPlatformUserService.updatePassword(rq);
    }


    @NotIntercept
    @ApiOperation(value = "获取个人信息")
    @RequestMapping(value = "/getSelfInfo", method = RequestMethod.GET)
    public ResponseResult<AuthPlatformUserInfo> getSelfInfo(@RequestHeader(ConstantsUtil.SYS_TOKEN) String sysToken) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userInfo);
    }

    @NotIntercept
    @RequestMapping(value = "/getUserInfo", method = RequestMethod.GET)
    @ApiOperation(value = "获取个人信息",hidden = true)
    public ResponseResult<AuthPlatformUserInfo> getUserInfo(@RequestParam(ConstantsUtil.SYS_TOKEN) String sysToken) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getUserInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, userInfo);
    }

    @NotIntercept
    @ApiOperation(value = "app端 - 获得当前用户的个人信息")
    @RequestMapping(value = "/client/getSelfInfo", method = RequestMethod.GET)
    public ResponseResult<AppUserInfo> getSelfInfoByApp(@RequestHeader(ConstantsUtil.SYS_TOKEN) String sysToken) {
        return authPlatformUserService.getSelfInfoByApp(sysToken);
    }

    @PostMapping(value = "/user")
    @ApiOperation(value = "新增用户")
    public ResponseResult<ResCodeEnum> addUser(@RequestHeader(ConstantsUtil.SYS_TOKEN) String sysToken,
                                               @RequestBody @Valid AddUserRQ rq){
        if(!Validator.isPassword(rq.getPassword())){
           return  ResponseResult.buildResponseResult(ResCodeEnum.PASSWORD_ERROR);
        }
        AuthPlatformUserInfo userInfo = authPlatformUserService.getUserInfo(sysToken);
        return authPlatformUserService.addUser(userInfo,rq);
    }


    @GetMapping("/users")
    @ApiOperation(value = "工厂配置 - 人员管理 - 列表查询")
    @ApiImplicitParam(value = "关键词",name = "keyword")
    public ResponseResult<List<UserDetailedDTO>> getUserByKeyWord(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                                  String keyword,Integer roleId, Page page) {
        log.info("关键词查询用户:keyWord={},roleId={}",  keyword,roleId);
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        //通过关键词查询用户信息
        return authPlatformUserService.getUserByKeyWord(userInfo,keyword, roleId,pagination);
    }


    @PutMapping("/user/{userId}")
    @ApiOperation(value = "工厂配置 - 人员管理 - 启用或禁用用户")
    @ApiImplicitParam(value = "用户id",name = "userId",paramType = "path")
    public ResponseResult<ResCodeEnum> updateUserStatus(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                                  @PathVariable("userId") Integer userId) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        //通过关键词查询用户信息
        return authPlatformUserService.updateUserStatus(userInfo,userId);
    }

    @PutMapping("/user")
    @ApiOperation(value = "工厂配置 - 人员管理 - 编辑用户信息")
    public ResponseResult<ResCodeEnum> updateUser(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                        @RequestBody @Valid EditUserRQ rq) {
        if(!Validator.isPassword(rq.getPassword())){
            return  ResponseResult.buildResponseResult(ResCodeEnum.PASSWORD_ERROR);
        }
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        //通过关键词查询用户信息
        return authPlatformUserService.updateUser(userInfo,rq);
    }


    @ApiOperation(value = "内部系统通过用户id获得用户信息")
    @RequestMapping(value = "/user/{userId}", method = RequestMethod.GET)
    public ResponseResult<AuthPlatformUserInfo> getUserInfoById(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                            @PathVariable("userId") Integer userId) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return authPlatformUserService.getUserInfoById(userInfo.getOrgId(),userId);
    }

    @ApiOperation(value = "内部系统通过用户id获得用户信息,返回一个map对象")
    @RequestMapping(value = "/simple/users", method = RequestMethod.POST)
    public ResponseResult<Map<Integer,AuthPlatformUserInfo>> getUserInfoById(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                                             @RequestBody UserIdsRQ rq) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        return authPlatformUserService.getSimpleUsers(userInfo.getOrgId(),rq.getUserIds());
    }
}

