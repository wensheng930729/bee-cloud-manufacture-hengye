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
 * @Description 物流报表销售运输运输台账
 * @author twp
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("采购运输报表")
public class BuyTransportReportDTO {

    @ApiModelProperty("供应商")
    private String supplierName;

    @ApiModelProperty("承运商")
    private String carrierName;

    @ApiModelProperty("采购合同号")
    private String contractNum;

    @ApiModelProperty("产品")
    private String productName;

    @ApiModelProperty("数量")
    private Integer quantity;

    @ApiModelProperty("发货日期")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    private Date departureTime;

    @ApiModelProperty("起点")
    private String startingPlace;

    @ApiModelProperty("终点")
    private String arrivalPlace;

    @ApiModelProperty("运输单价")
    private BigDecimal unitPrice;

}
