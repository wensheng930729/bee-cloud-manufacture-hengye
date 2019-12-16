package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;
import java.util.Map;

/**
 * @ClassName: ReportFormFieldsDTO
 * @Description:
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("报表抬头合计字段返回数据")
public class ReportFormFieldsTotalDTO {

    @ApiModelProperty("固定字段")
    private List<ReportFormFieldsFixedDTO> fieldsFixed;

    @ApiModelProperty("动态字段-输出项")
    private List<Map<String,Object>> assayItem;

}
