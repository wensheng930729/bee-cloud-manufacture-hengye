package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName ReportFormAmountByShift
 * @Description 按班次产量统计
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("按班次产量统计")
public class ReportFormAmountByShift {

    @ApiModelProperty("规格id")
    private Integer productSpecId;

    @ApiModelProperty("1班产量")
    private BigDecimal shiftOne;

    @ApiModelProperty("2班产量")
    private BigDecimal shiftTwo;

    @ApiModelProperty("3班产量")
    private BigDecimal shiftThree;

}
