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
 * @Description BI采购付款情况DTO2
 * @Date 2019/10/16 9:31
 */
@Data
@Accessors(chain = true)
@ApiModel("BI采购总额数量占比DTO")
public class BuyPurchaseAmountRatioDTO {

    @ApiModelProperty("供应商id")
    private Integer supplierId;

    @ApiModelProperty("供应商名称")
    private String supplierName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("数量")
    private BigDecimal amount;


}
