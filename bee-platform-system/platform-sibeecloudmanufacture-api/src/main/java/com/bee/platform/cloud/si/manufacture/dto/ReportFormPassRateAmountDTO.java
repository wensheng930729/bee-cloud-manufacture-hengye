package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName ReportFormPassRateAmountDTO
 * @Description 矿热炉按月份统计的数量
 * @author qhwang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("矿热炉按月份统计的数量")
public class ReportFormPassRateAmountDTO {

    @ApiModelProperty("矿热炉id")
    private String furnaceId;

    @ApiModelProperty("数量")
    private BigDecimal amount;

}
