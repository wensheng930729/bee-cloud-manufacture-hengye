package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "吨袋信息")
public class ProBaggingInfo {

    @ApiModelProperty("吨袋编号")
    private String baggingCode;

    @ApiModelProperty("数量")
    private BigDecimal amount;

    @ApiModelProperty("每个吨袋的装袋信息")
    private Date realBaggingDate;
}
