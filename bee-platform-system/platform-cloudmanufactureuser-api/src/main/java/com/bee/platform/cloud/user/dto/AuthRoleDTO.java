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
 * @description:
 * @author: junyang.li
 * @create: 2019-09-19 16:47
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("角色信息返回对象")
public class AuthRoleDTO implements Serializable {

    private static final long serialVersionUID = 532884943083157169L;

    @ApiModelProperty("角色id")
    private Integer roleId;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("描述")
    private String describe;

    @ApiModelProperty("角色类型")
    private String roleType;

    @ApiModelProperty("是否允许修改 0 否 ，1 允许")
    private Integer canEdit;
}
