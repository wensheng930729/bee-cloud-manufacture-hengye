package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author jie.zhang
 * @version 1.0.0
 * @ClassName ReportFormTestProduceDTO
 * @Description 采购报表返回
 */
@Data
@Accessors(chain = true)
@ApiModel("现存明细总返回")
public class ReportFormExistingAllDTO {

    @ApiModelProperty("报表数据")
    private List<ReportFormExistingDeatilsDTO> data;

    @ApiModelProperty("统计值")
    private ReportFormExistingTotalDTO dataSum;
}
