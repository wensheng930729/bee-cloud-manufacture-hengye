package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName ReportFormOutputStatisticsCommonFields
 * @Description 产量统计公共字段
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("产量统计公共字段")
public class ReportFormOutputStatisticsCommonFields {

    @ApiModelProperty("产量")
    private BigDecimal output;

    @ApiModelProperty("百分比")
    private BigDecimal percent;

    @ApiModelProperty("累计产量")
    private BigDecimal outputCumulative;

}
