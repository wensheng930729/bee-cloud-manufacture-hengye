package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @Description 原料日报表返回
 * @author chenxm66777123
 * @Date 2019/10/22 9:57
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("原料日报表总返回")
public class ReportRawMaterialAllDTO {

    @ApiModelProperty("报表数据")
    private List<ReportRawMaterialDTO> data;

    @ApiModelProperty("统计值")
    private ReportRawMaterialTotalDTO dataSum;

}
