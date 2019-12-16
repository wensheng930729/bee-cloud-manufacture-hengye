package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName ReportFormTestQualityTestAssayDTO
 * @Description 报表质检化验
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("报表质检化验")
public class ReportFormTestQualityTestAssayDTO {

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("化验项")
    private String assayItem;

    @ApiModelProperty("化验项结果")
    private Double assayValue;

}
