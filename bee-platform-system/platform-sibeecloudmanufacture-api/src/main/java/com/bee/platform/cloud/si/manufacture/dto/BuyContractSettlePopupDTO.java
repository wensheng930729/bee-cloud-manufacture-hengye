package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @ClassName BuyContractSettlePopupDTO
 * @Description 合同结算详情弹窗
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同结算详情弹窗")
public class BuyContractSettlePopupDTO {

    @ApiModelProperty("合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("供应商")
    private String supplierName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("采购单价")
    private BigDecimal unitPrice;

    @ApiModelProperty("磅单结算")
    private List<BuyContractSettlePopupCarDTO> settles;

    @ApiModelProperty("净重")
    private BigDecimal weightNet;

    @ApiModelProperty("扣重重量")
    private BigDecimal weightDeduct;

    @ApiModelProperty("已结算：结算重量")
    private BigDecimal weightSettle;

    @ApiModelProperty("已结算：结算单价")
    private BigDecimal unitPriceSettlement;

    @ApiModelProperty("结算总价")
    private BigDecimal amountSettlement;
}
