package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import java.util.List;

/**
 * @ClassName ReportFormSaleTotalDTO
 * @Description 销售报表返回
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售报表返回")
public class ReportFormSaleTotalDTO {

    @ApiModelProperty("销售报表")
    private List<ReportFormSaleDTO> data;

    @ApiModelProperty("销售报表统计值")
    private ReportFormSaleSumDTO dataSum;

}
