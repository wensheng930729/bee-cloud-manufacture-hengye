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
 * @description:
 * @author: junyang.li
 * @create: 2019-09-19 15:04
 **/
@Getter
@Setter
@Accessors(chain = true)
@ToString
@NoArgsConstructor
@ApiModel("用户验证码登录相关参数")
public class CodeLoginRQ implements Serializable {

    private static final long serialVersionUID = 2781594682848847998L;

    @ApiModelProperty("用户账号")
    @NotEmpty(message = "账号不能为空")
    private String username;

    @ApiModelProperty(value = "获取的验证码")
    @NotEmpty(message = "验证码不能为空")
    private String code;

    @ApiModelProperty(value = "当前登录的客户端唯一标识")
    private String currentClientId;
}
