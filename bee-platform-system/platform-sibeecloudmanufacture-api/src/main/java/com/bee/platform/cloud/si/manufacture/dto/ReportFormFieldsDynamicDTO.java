package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: ReportFormFieldsDynamicDTO
 * @Description:
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("报表抬头动态字段返回数据")
public class ReportFormFieldsDynamicDTO {

    @ApiModelProperty("字段名")
    private String assayItem;

}
