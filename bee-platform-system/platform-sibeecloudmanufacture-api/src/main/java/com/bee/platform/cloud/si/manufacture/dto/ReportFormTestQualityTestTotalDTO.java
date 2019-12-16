package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import java.util.List;
import java.util.Map;

/**
 * @ClassName ReportFormTestProduceDTO
 * @Description 质检报表返回
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("质检报表返回")
public class ReportFormTestQualityTestTotalDTO {

    @ApiModelProperty("data")
    private List<ReportFormTestQualityTestDTO> data;

    @ApiModelProperty("平均值")
    private Map<String,Object> dataAvg;

}
