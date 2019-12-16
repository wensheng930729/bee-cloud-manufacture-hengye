package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

@Data
@Accessors(chain = true)
@ApiModel("数据总览DTO")
public class DataScreenDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("采购金额每日统计")
    private BigDecimal buyDay;
    @ApiModelProperty("采购金额每年统计")
    private BigDecimal buyYear;
    @ApiModelProperty("销售金额每日统计")
    private BigDecimal saleDay;
    @ApiModelProperty("销售金额每年统计")
    private BigDecimal saleYear;
    @ApiModelProperty("生产金额每日统计")
    private BigDecimal proDay;
    @ApiModelProperty("生产金额每年统计")
    private BigDecimal proYear;

}
