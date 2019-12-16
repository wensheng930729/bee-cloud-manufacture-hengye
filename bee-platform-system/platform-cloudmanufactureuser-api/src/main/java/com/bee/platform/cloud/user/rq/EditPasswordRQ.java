package com.bee.platform.cloud.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

/**
 * @description: 修改密码
 * @author: junyang.li
 * @create: 2019-09-19 17:33
 **/
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
@ApiModel("用户修改密码参数")
public class EditPasswordRQ implements Serializable {

    private static final long serialVersionUID = 1553680562252573700L;

    @ApiModelProperty("用户账号")
    @NotEmpty(message = "用户账号不能为空")
    private String username;

    @ApiModelProperty("验证码")
    @NotEmpty(message = "用户账号不能为空")
    private String code;

    @ApiModelProperty("新密码")
    @NotEmpty(message = "用户账号不能为空")
    private String newPassword;
}
