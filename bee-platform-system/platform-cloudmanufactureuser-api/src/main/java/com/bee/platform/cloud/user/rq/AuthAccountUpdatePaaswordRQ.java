package com.bee.platform.cloud.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import java.io.Serializable;

/**
 * @description: 修改用户密码的参数
 * @author: junyang.li
 * @create: 2019-09-25 17:27
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@ApiModel("客户或供应商修改用户密码的参数")
public class AuthAccountUpdatePaaswordRQ implements Serializable {

    private static final long serialVersionUID = -4498528945369263494L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;

//    @ApiModelProperty("用户id")
//    @NotNull(message = "用户id不能为空")
//    private Integer userId;

    @ApiModelProperty("密码")
    @NotEmpty(message = "密码不能为空")
    @Length(max = 60,message = "密码限制60个字符")
    @Pattern(regexp ="^(?=.*[a-zA-Z])(?=.*[0-9])[a-zA-Z0-9]{6,16}$")
    private String password;
}
