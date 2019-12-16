package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @Description 原料日报表返回
 * @author chenxm66777123
 * @Date 2019/10/22 9:57
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("原料日报表返回")
public class ReportRawMaterialDTO {

    @ApiModelProperty("产品")
    private String productName;

    @ApiModelProperty("规格")
    private String productSpecName;

    @ApiModelProperty("原有库存")
    private BigDecimal originalStock;

    @ApiModelProperty("今日入库")
    private BigDecimal todayIntoStock;

    @ApiModelProperty("今日用量")
    private BigDecimal todayUsed;

    @ApiModelProperty("损耗")
    private BigDecimal loss;

    @ApiModelProperty("今日库存")
    private BigDecimal todayStock;

}
