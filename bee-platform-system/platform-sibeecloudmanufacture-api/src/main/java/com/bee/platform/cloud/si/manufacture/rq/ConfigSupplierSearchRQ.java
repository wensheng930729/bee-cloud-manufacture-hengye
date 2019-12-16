package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel("客户管理列表搜索请求参数")
public class ConfigSupplierSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    /**
     * 供应商名称
     */
    @ApiModelProperty("供应商名称")
    private String name;

    /**
     * 供应商类别（0核心供应商 1战略供应商  2储备供应商）
     */
    @ApiModelProperty("供应商类别（0核心供应商 1战略供应商  2储备供应商）")
    private Integer category;
    /**
     * 启用状态（ 0禁用 1启用 ）
     */
    @ApiModelProperty("启用状态（ 0禁用 1启用 ）")
    private Integer status;






}
