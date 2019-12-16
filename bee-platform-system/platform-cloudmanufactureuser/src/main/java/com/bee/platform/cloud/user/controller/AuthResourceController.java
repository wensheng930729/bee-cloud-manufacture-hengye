package com.bee.platform.cloud.user.controller;


import com.bee.platform.cloud.user.dto.FunctionDTO;
import com.bee.platform.cloud.user.service.AuthPlatformUserService;
import com.bee.platform.cloud.user.service.AuthResourceService;
import com.bee.platform.cloud.user.service.resource.CancelResourceStrategyService;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.entity.*;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * 资源表 前端控制器
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-20
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/auth")
@Api(value = "新权限：资源相关接口", tags = "新权限：资源相关接口")
public class AuthResourceController {

    @Autowired
    private AuthResourceService authResourceService;
    @Autowired
    private AuthPlatformUserService authPlatformUserService;
    @Autowired
    private CancelResourceStrategyService cancelResourceStrategyService;

    @ApiOperation(value = "资源-功能模块列表查询", notes = "资源-功能模块列表查询")
    @GetMapping(value = "/resources")
    public ResponseResult<FunctionDTO> getRoleList() {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, cancelResourceStrategyService.selectFunction());
    }

    @ApiOperation(value = "资源-获得所有的菜单资源树", notes = "资源-获得所有的菜单资源树")
    @GetMapping(value = "/resource/tree")
    public ResponseResult<List<AuthResourceInfo>> getResourceTree() {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,authResourceService.getResourceTree());
    }


    @NotIntercept
    @ApiOperation(value = "获得当前用户可访问的菜单资源", notes = "获得当前用户可访问的菜单资源")
    @GetMapping(value = "/resource/user")
    public ResponseResult<List<AuthResourceInfo>> getUserResource(@RequestHeader("sysToken")String sysToken,
                                                                  @RequestHeader("cloudMafType")String cloudMafType) {
        AuthPlatformUserInfo authPlatformUserInfo=authPlatformUserService.getSelfInfo(sysToken);
        AuthRoleInfo roleInfo = authPlatformUserInfo.getRoleInfo();
        //应用标识判空
        if(StringUtils.isEmpty(cloudMafType)){
            log.info("查询当前用户的菜单时未获取到子应用标识，无法查询。当前用户是:{}",authPlatformUserInfo.getUsername());
            return ResponseResult.buildResponseResult(ResCodeEnum.MISS_NECESSARY_PARAM);
        }
        return authResourceService.getUserResource(roleInfo.getRoleId(),cloudMafType);
    }
}

