package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author jie.zhang
 * @version 1.0.0
 * @ClassName ReportFormTestProduceDTO
 * @Description 采购报表返回
 */
@Data
@Accessors(chain = true)
@ApiModel("现存明细统计字段返回")
public class ReportFormExistingTotalDTO {

    @ApiModelProperty("数量")
    private BigDecimal amount;
}
