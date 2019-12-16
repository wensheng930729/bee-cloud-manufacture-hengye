package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author liliang
 * @version 1.0.0
 * @ClassName BuyPurchaseMoneyRatioDTO
 * @Description BI采购总额DTO
 * @Date 2019/10/16 9:31
 */
@Data
@Accessors(chain = true)
@ApiModel("BI采购总额占比DTO")
public class BuyPurchaseMoneyRatioDTO {

    @ApiModelProperty("供应商id")
    private Integer supplierId;

    @ApiModelProperty("供应商名称")
    private String supplierName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("金额")
    private BigDecimal money;

}
