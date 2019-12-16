package com.bee.platform.cloud.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 上下游账号管理
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-18
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("上下游账号管理返回信息")
public class AuthUpAndDownstreamAccountUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;
    /**
     * 姓名
     */
    @ApiModelProperty("姓名")
    @NotEmpty(message = "姓名不能为空")
    private String name;
    /**
     * 注册手机号
     */
    @ApiModelProperty("注册手机号")
    @NotEmpty(message = "注册手机号不能为空")
    @Max(value = 11,message = "注册手机号为11位")
    @Min(value = 11,message = "注册手机号为11位")
    private String phone;
    /**
     * 邮箱
     */
    @ApiModelProperty("邮箱")
    private String mailbox;
    /**
     * 启用状态（ 0禁用 1启用 ）
     */
    @ApiModelProperty("启用状态（ 0禁用 1启用 ）")
    @NotNull(message = "启用状态不能为空")
    private Integer status;





}
