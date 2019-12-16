package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "配料明细返回信息")
public class ProIngredientDetailDTO implements Serializable {
    private static final long serialVersionUID = 479351191793976410L;

    @ApiModelProperty("配料主表id")
    private Long ingredientId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("产品单位")
    private String unit;

    @ApiModelProperty("称重设备id")
    private Integer weighDeviceId;

    @ApiModelProperty("称重设备名称")
    private String weighDeviceName;

    @ApiModelProperty("产品数量")
    private BigDecimal num;

    @ApiModelProperty("仓库id")
    private Integer warehouseId;

    @ApiModelProperty("仓库名称")
    private String warehouseName;

}
