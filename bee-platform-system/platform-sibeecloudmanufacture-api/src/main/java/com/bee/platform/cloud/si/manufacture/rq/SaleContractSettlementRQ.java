package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName SaleContractSettlementRQ
 * @Description 合同结算数据新增参数
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/9/29 10:47
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "合同结算数据新增参数")
public class SaleContractSettlementRQ {

    @ApiModelProperty("销售合同业务id")
    @NotEmpty(message="销售合同业务id不能为空")
    private String contractBusinessId;

    @ApiModelProperty("结算时间")
    @NotEmpty(message="结算时间不能为空")
    private String settleTime;

    @ApiModelProperty("收货数量")
    @NotNull(message="收货数量不能为空")
    private BigDecimal weightReceive;

    @ApiModelProperty("结算数量")
    @NotNull(message="结算数量不能为空")
    private BigDecimal weightSettle;

    @ApiModelProperty("结算水分")
    @NotNull(message="结算水分不能为空")
    private BigDecimal waterContentSettle;

    @ApiModelProperty("结算单价")
    @NotNull(message="结算单价不能为空")
    private BigDecimal unitPriceSettlement;

    @ApiModelProperty("结算金额")
    @NotNull(message="结算金额不能为空")
    private BigDecimal amountSettlement;

}
