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
 * 产品规格表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品规格请求参数")
public class ConfigProductSpecRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;
    /**
     * 规格名称
     */
    @ApiModelProperty("规格名称")
    @NotEmpty(message = "规格名称不能为空")
    private String specName;
    /**
     * 合格线（0 否 1 是）
     */
    @ApiModelProperty("合格线（0 否 1 是）")
    @NotNull(message = "合格徐不能为空")
    private Integer qualifiedLine;
    /**
     * 状态(1-启用 ,0-禁用)
     */
    @ApiModelProperty("状态(1-启用 ,0-禁用)")
    @NotNull(message = "状态不能为空")
    private Integer status;
    /**
     * 排序
     */
    @ApiModelProperty("排序")
    @NotNull(message = "排序不能为空")
    private Integer sort;





}
