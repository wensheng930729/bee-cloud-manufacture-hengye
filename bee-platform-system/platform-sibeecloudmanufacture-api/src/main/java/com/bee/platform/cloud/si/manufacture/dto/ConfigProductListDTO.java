package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品列表返回信息")
public class ConfigProductListDTO implements Serializable {

    private static final long serialVersionUID = 1L;

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
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String name;
    /**
     * 产品logo
     */
    @ApiModelProperty("产品logo")
    private String logo;
    /**
     * 单位code
     */
    @ApiModelProperty("单位code")
    private String unitCode;
    /**
     * 单位value
     */
    @ApiModelProperty("单位value")
    private String unitValue;
    /**
     * 产品类别
     */
    @ApiModelProperty("产品类别")
    private Integer categoryId;
    /**
     * 产品类别名称
     */
    @ApiModelProperty("产品类别名称")
    private String categoryName;
    /**
     * 状态:1-启用,0-禁用
     */
    @ApiModelProperty("状态:1-启用,0-禁用")
    private Integer status;

    /**
     * 是否是标准品（0 否 1 是）
     */
    @ApiModelProperty("是否是标准品（0 否 1 是）")
    private Integer standard;



}
