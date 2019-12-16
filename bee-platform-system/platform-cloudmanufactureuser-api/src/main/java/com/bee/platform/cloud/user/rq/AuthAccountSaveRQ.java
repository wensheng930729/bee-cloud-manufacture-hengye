package com.bee.platform.cloud.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.*;
import java.io.Serializable;

/**
 * <p>
 * 客户账号和供应商账号
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-18
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("客户账号和供应商账号保存请求参数")
public class AuthAccountSaveRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    /**
     * 关联id(客户id 或者 供应商id）
     */
    @ApiModelProperty("关联id(客户id 或者 供应商id）")
    @NotNull(message = "关联id不能为空")
    private Integer relatedId;
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
    @Length(max = 11,min = 11,message = "手机号为11位")
    @Pattern(regexp ="^1[3456789]\\d{9}$",message = "手机号格式不正确")
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
    /**
     * 职务
     */
    @ApiModelProperty("职务")
    private String job;
    /**
     * 是否是默认公司(0 不是 1 是)
     */
    @ApiModelProperty("是否是默认公司(0 不是 1 是)")
    private Integer defaultEnterprise;
    /**
     * 分类（0 客户 1供应商）
     */
    @ApiModelProperty("分类（0 客户 1供应商）")
    @NotNull(message = "账号分类不能为空")
    @Min(value = 0,message = "分类为0或1")
    @Max(value = 1,message = "分类为0或1")
    private Integer type;



}
