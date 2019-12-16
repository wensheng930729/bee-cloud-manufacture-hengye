package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * <p>
 * 客户管理
 * </p>
 *
 * @author MP123
 * @since 2019-10-09
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("客户管理修改请求参数")
public class ConfigCustomerUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;

    /**
     * 客户名称
     */
    @ApiModelProperty("客户名称")
    @NotEmpty(message = "客户名称不能为空")
    private String name;
    /**
     * 联系方式
     */
    @ApiModelProperty("联系方式")
//    @NotEmpty(message = "客户名称不能为空")
    private String telephone;
    /**
     * 地址
     */
    @ApiModelProperty("地址")
    private String address;
    /**
     * 公司邮箱
     */
    @ApiModelProperty("公司邮箱")
    private String mailbox;
    /**
     * 客户类别（0核心客户 1战略客户  2一般客户）
     */
    @ApiModelProperty("客户类别（0核心客户 1战略客户  2一般客户）")
    @NotNull(message = "客户类别不能为空")
    private Integer category;
    /**
     * 启用状态（ 0禁用 1启用 ）
     */
    @ApiModelProperty("启用状态（ 0禁用 1启用 ）")
    @NotNull(message = "启用状态不能为空")
    private Integer status;
    /**
     * 相关联系人
     */
    @ApiModelProperty("相关联系人")
    private String relatedContacts;
    /**
     * 电话号码
     */
    @ApiModelProperty("电话号码")
    private String phoneNumber;
    /**
     * 银行账号
     */
    @ApiModelProperty("银行账号")
    private String bankAccount;
    /**
     * 开户银行
     */
    @ApiModelProperty("开户银行")
    private String bank;
    /**
     * 公司税号
     */
    @ApiModelProperty("公司税号")
    private String taxNumber;
    /**
     * 开票地址
     */
    @ApiModelProperty("开票地址")
    private String billingAddress;





}
