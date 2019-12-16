package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-09-25 14:38
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("用户列表查询返回数据")
public class UserDetailedDTO implements Serializable {

    private static final long serialVersionUID = -7304993063685529489L;

    @ApiModelProperty("用户id")
    private Integer userId;

    @ApiModelProperty("手机号")
    private String username;

    @ApiModelProperty("姓名")
    private String name;

    @ApiModelProperty("角色id")
    private Integer roleId;

    @ApiModelProperty("角色名称")
    private String roleName;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("返回-状态")
    private String status;

    @ApiModelProperty("不返回字段")
    private Integer state;

    @ApiModelProperty("是否可编辑")
    private Integer canEdit;
}
