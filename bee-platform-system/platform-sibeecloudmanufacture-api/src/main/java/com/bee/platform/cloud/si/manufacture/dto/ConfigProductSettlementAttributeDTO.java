package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品结算属性
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品结算属性返回信息")
public class ConfigProductSettlementAttributeDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    private Integer productId;
    /**
     * 结算项
     */
    @ApiModelProperty("结算项")
    private String settlementItem;
    /**
     * 字段限制
     */
    @ApiModelProperty("字段限制")
    private Integer fieldLimit;
    /**
     * 单位
     */
    @ApiModelProperty("单位")
    private String unit;
    /**
     * 状态 1启用 0未启用
     */
    @ApiModelProperty("状态 1启用 0未启用")
    private Integer status;


}
