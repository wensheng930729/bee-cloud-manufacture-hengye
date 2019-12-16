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
@ApiModel("客户账号和供应商搜索请求参数")
public class AuthCustomerAndSupplierSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;


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
     * 名称
     */
    @ApiModelProperty("名称")
    private String name;

    /**
     * 分类（0 客户 1供应商）
     */
    @ApiModelProperty("分类（0 客户 1供应商）")
    @Min(value = 0,message = "分类为0或1")
    @Max(value = 1,message = "分类为0或1")
    private Integer type;



}
