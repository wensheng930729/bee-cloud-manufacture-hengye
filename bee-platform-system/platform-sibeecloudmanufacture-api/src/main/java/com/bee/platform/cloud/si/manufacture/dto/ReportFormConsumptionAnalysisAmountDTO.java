package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName ReportFormConsumptionAnalysisAmountDTO
 * @Description 产成品原料按炉次班次统计的数量
 * @author qhwang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("产成品原料按炉次班次统计的数量")
public class ReportFormConsumptionAnalysisAmountDTO {

    @ApiModelProperty("原料id")
    private Integer productId;

    @ApiModelProperty("原料名称")
    private String productName;

    @ApiModelProperty("一班消耗数量")
    private BigDecimal consume1;

    @ApiModelProperty("二班消耗数量")
    private BigDecimal consume2;

    @ApiModelProperty("三班消耗数量")
    private BigDecimal consume3;

}
