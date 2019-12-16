package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName ReportFormSaleSumDTO
 * @Description 销售报表合计
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售报表合计")
public class ReportFormSaleSumDTO {

    @ApiModelProperty("合同数量合计")
    private BigDecimal quantity;

    @ApiModelProperty("合同金额统计")
    private BigDecimal amount;

    @ApiModelProperty("到厂数量")
    private BigDecimal weightReceive;

    @ApiModelProperty("结算数量")
    private BigDecimal weightSettle;

    @ApiModelProperty("结算金额")
    private BigDecimal amountSettlementTotal;

    @ApiModelProperty("回款金额")
    private BigDecimal amountCollectionTotal;

}
