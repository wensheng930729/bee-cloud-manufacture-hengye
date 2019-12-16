package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

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
@ApiModel("客户账号和供应商账号返回信息")
public class AuthAccountDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 所属企业id
     */
    @ApiModelProperty("所属企业id")
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @ApiModelProperty("工厂id")
    private Integer factoryId;
    /**
     * 关联id(客户id 或者 供应商id）
     */
    @ApiModelProperty("关联id(客户id 或者 供应商id）")
    private Integer relatedId;
    /**
     * 姓名
     */
    @ApiModelProperty("姓名")
    private String name;
    /**
     * 注册手机号
     */
    @ApiModelProperty("注册手机号")
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
    private Integer type;
    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;


}
