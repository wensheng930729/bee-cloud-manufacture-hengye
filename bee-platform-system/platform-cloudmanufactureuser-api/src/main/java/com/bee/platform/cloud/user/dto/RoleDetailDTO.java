package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @description: 角色详细数据
 * @author: junyang.li
 * @create: 2019-09-24 13:29
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("角色详细数据")
public class RoleDetailDTO implements Serializable {

    private static final long serialVersionUID = -8074477679085958235L;

    @ApiModelProperty("角色id")
    private Integer roleId;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("角色描述")
    private String describe;

    @ApiModelProperty("APP模块权限")
    private List<FunctionListDTO> app;

    @ApiModelProperty("web模块权限")
    private List<FunctionListDTO> web;

    @ApiModelProperty("bi模块权限")
    private List<FunctionListDTO> bi;

    public RoleDetailDTO(Integer roleId, String roleName, String describe) {
        this.roleId = roleId;
        this.roleName = roleName;
        this.describe = describe;
    }
}
