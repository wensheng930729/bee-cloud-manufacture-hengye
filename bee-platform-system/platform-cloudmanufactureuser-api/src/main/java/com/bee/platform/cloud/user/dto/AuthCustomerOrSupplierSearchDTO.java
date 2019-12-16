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
@ApiModel("客户账号或供应商列表搜索返回信息")
public class AuthCustomerOrSupplierSearchDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 姓名
     */
    @ApiModelProperty("公司名称")
    private String name;

    /**
     * 分类（0 客户 1供应商）
     */
    @ApiModelProperty("分类（0 客户 1供应商）")
    private Integer type;


    @ApiModelProperty("人数")
    private Integer num;


}
