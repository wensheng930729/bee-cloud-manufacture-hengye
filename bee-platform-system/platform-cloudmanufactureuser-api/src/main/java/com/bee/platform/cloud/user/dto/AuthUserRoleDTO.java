package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 用户角色信息
 * @author: junyang.li
 * @create: 2019-09-19 13:28
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("用户角色信息")
public class AuthUserRoleDTO implements Serializable {

    private static final long serialVersionUID = 1713563985919375772L;

    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("角色id")
    private Integer roleId;

    @ApiModelProperty("企业id")
    private Integer enterpriseId;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("角色类型")
    private String roleType;

    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("是否启用：1启用，0禁用")
    private Integer status;
}
