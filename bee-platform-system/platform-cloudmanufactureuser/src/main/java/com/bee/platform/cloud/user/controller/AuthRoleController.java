package com.bee.platform.cloud.user.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.user.dto.AuthRoleDTO;
import com.bee.platform.cloud.user.dto.RoleDetailDTO;
import com.bee.platform.cloud.user.rq.CreateRoleRQ;
import com.bee.platform.cloud.user.service.AuthPlatformUserService;
import com.bee.platform.cloud.user.service.AuthRoleService;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.PageUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 角色表 前端控制器
 * </p>
 *
 * @author zhigang.zhou123
 * @since 2019-09-19
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "新权限：角色相关接口", tags = "新权限：角色相关接口")
@RequestMapping("/auth")
public class AuthRoleController {

    @Autowired
    private AuthRoleService authRoleService;

    @Autowired
    private AuthPlatformUserService authPlatformUserService;

    @ApiOperation(value = "工厂配置-权限设置-角色列表查询", notes = "角色查询")
    @GetMapping(value = "/roles")
    public ResponseResult<List<AuthRoleDTO>> getRoleList(@RequestHeader("sysToken")String sysToken,
                                                         String roleName,Page page) {
        AuthPlatformUserInfo userInfo=authPlatformUserService.getUserInfo(sysToken);
        Pagination pagination= PageUtils.transFromPage(page);
        return authRoleService.selectByKeyword(userInfo,roleName,pagination);
    }

    @ApiOperation(value = "工厂配置-权限设置-创建/修改角色", notes = "创建/修改角色")
    @PostMapping(value = "/role")
    public ResponseResult<ResCodeEnum> editRole(@RequestHeader("sysToken") String sysToken,
                                                        @RequestBody @Valid CreateRoleRQ rq) {
        AuthPlatformUserInfo userInfo=authPlatformUserService.getUserInfo(sysToken);
        return authRoleService.editRole(userInfo,rq);
    }


    @DeleteMapping(value = "/role/{roleId}")
    @ApiOperation(value = "工厂配置-权限设置-删除角色", notes = "删除角色")
    @ApiImplicitParam(value = "角色id",name = "roleId",dataType = "path")
    public ResponseResult<ResCodeEnum> deleteRole(@RequestHeader("sysToken") String sysToken,
                                                  @PathVariable("roleId") Integer roleId) {
        AuthPlatformUserInfo userInfo=authPlatformUserService.getUserInfo(sysToken);
        return authRoleService.deleteRole(userInfo,roleId);
    }


    @GetMapping(value = "/role/{roleId}")
    @ApiOperation(value = "工厂配置-权限设置-通过角色id查询角色详细", notes = "通过角色id查询角色详细")
    @ApiImplicitParam(value = "角色id",name = "roleId",dataType = "path")
    public ResponseResult<RoleDetailDTO> getRoleDetailById(@RequestHeader("sysToken")String sysToken,
                                                           @PathVariable("roleId") Integer roleId) {
        AuthPlatformUserInfo userInfo=authPlatformUserService.getUserInfo(sysToken);
        return authRoleService.getRoleDetailById(roleId,userInfo.getOrgId());
    }

    @ApiOperation(value = "获得当前用户可访问的结算单类型", notes = "获得当前用户可访问的结算单类型")
    @GetMapping(value = "/role/settlement/auth")
    public ResponseResult<List<String>> getSettlementAuth(@RequestHeader("sysToken")String sysToken) {
        AuthPlatformUserInfo authPlatformUserInfo=authPlatformUserService.getSelfInfo(sysToken);
        AuthRoleInfo roleInfo = authPlatformUserInfo.getRoleInfo();
        return authRoleService.getSettlementAuth(roleInfo.getRoleId());
    }
}

