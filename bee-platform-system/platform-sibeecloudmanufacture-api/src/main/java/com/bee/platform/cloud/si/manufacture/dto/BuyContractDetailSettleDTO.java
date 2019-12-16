package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName BuyContractDetailSettleDTO
 * @Description 合同内容结算详情情况
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同内容结算详情情况")
public class BuyContractDetailSettleDTO {

    @ApiModelProperty("结算业务id")
    private String contractSettlementBusinessId;

    @ApiModelProperty("结算序号")
    private String serialNum;

    @ApiModelProperty("结算时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy年MM月dd日")
    private Date settleTime;

    @ApiModelProperty("收货数量")
    private BigDecimal weightReceive;

    @ApiModelProperty("结算数量")
    private BigDecimal weightSettle;

    @ApiModelProperty("结算水分")
    private BigDecimal waterContentSettle;

    @ApiModelProperty("单位")
    private String unitValue;

    @ApiModelProperty("结算单价")
    private BigDecimal unitPriceSettlement;

    @ApiModelProperty("结算金额")
    private BigDecimal amountSettlement;

    @ApiModelProperty("结算状态 0重量确认 1价格确认(置灰) 2确认完成(置灰)")
    private Integer settlementStatus;

}
