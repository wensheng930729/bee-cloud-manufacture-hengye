package com.bee.platform.cloud.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import java.io.Serializable;

/**
 * @description: 编辑用户传入参数
 * @author: junyang.li
 * @create: 2019-09-25 16:32
 **/
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
@ApiModel("编辑用户传入参数")
public class EditUserRQ implements Serializable {

    private static final long serialVersionUID = 4764523860562056378L;

    @ApiModelProperty("用户id")
    @NotNull(message = "用户id不能为空")
    private Integer userId;

    @ApiModelProperty("员工姓名")
    @NotEmpty(message = "员工姓名不能为空")
    @Length(max = 20,message = "员工姓名限制20个字符")
    private String name;

    @ApiModelProperty("员工电话")
    @NotEmpty(message = "员工电话不能为空")
    @Length(max = 11,message = "员工电话限制11个字符")
    @Pattern(regexp = "^(1)\\d{10}$",message = "请输入正确的手机号")
    private String username;

    @ApiModelProperty("角色id")
    @NotNull(message = "角色不能为空")
    @Range(message = "角色超出限制")
    private Integer roleId;

    @ApiModelProperty("密码")
    @Length(max = 60,message = "密码限制60个字符")
    private String password;
}
