package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @ClassName ReportFormTestProduceDTO
 * @Description 采购报表返回
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("采购报表返回")
public class ReportFormBuyDTO {

    @ApiModelProperty("合同id")
    private Integer id;

    @ApiModelProperty("供应商")
    private String supplierName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("合同编号")
    private String contractNum;

    @ApiModelProperty("签订日期")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    private Date signDate;

    @ApiModelProperty("合同数量")
    private BigDecimal quantity;

    @ApiModelProperty("合同金额")
    private BigDecimal amount;

    @ApiModelProperty("质量要求")
    private String qualityRequirement;

    @ApiModelProperty("合同单价")
    private BigDecimal unitPrice;

    @ApiModelProperty("发货数量")
    private BigDecimal arrivalVolume;

    @ApiModelProperty("未发数量")
    private BigDecimal notArrivalVolume;

    @ApiModelProperty("收货数量")
    private BigDecimal weightReceive;

    @ApiModelProperty("完成数量")
    private BigDecimal completedVolume;

    @ApiModelProperty("结算数量")
    private BigDecimal weightSettle;

    @ApiModelProperty("重量盈亏")
    private BigDecimal gainOrLossWeight;

    @ApiModelProperty("结算金额")
    private BigDecimal amountSettlementTotal;

    @ApiModelProperty("已付款金额")
    private BigDecimal amountPaymentTotal;

    @ApiModelProperty("合同完成")
    private Integer completed;

    @ApiModelProperty("合同完成状态")
    private String completedStatus;

    /**
     * 未发量
     */
    private BigDecimal undeliveredVolume;

}
