package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName ReportFormSaleDTO
 * @Description 销售报表返回
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售报表返回")
public class ReportFormSaleDTO {

    @ApiModelProperty("合同id")
    private Integer id;

    @ApiModelProperty("客户")
    private String customerName;

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

    @ApiModelProperty("到厂数量")
    private BigDecimal weightReceive;

    @ApiModelProperty("完成数量")
    private BigDecimal completedVolume;

    @ApiModelProperty("结算数量")
    private BigDecimal weightSettle;

    @ApiModelProperty("重量盈亏")
    private BigDecimal gainOrLossWeight;

    @ApiModelProperty("结算金额")
    private BigDecimal amountSettlementTotal;

    @ApiModelProperty("回款金额")
    private BigDecimal amountCollectionTotal;

    @ApiModelProperty("合同完成")
    private Integer completed;

    @ApiModelProperty("合同完成状态")
    private String completedStatus;

    /**
     * 已收货量
     */
    private BigDecimal receivedVolume;
    /**
     * 已发量
     */
    private BigDecimal issuedVolume;
    /**
     * 未发量
     */
    private BigDecimal undeliveredVolume;

}
