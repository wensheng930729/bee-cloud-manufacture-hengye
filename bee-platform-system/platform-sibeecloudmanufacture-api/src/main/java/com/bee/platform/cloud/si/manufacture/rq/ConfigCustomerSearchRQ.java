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
public class ConfigCustomerSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    /**
     * 客户名称
     */
    @ApiModelProperty("客户名称")
    private String name;

    /**
     * 客户类别（0核心客户 1战略客户  2一般客户）
     */
    @ApiModelProperty("客户类别（0核心客户 1战略客户  2一般客户）")
    private Integer category;
    /**
     * 启用状态（ 0禁用 1启用 ）
     */
    @ApiModelProperty("启用状态（ 0禁用 1启用 ）")
    private Integer status;






}
