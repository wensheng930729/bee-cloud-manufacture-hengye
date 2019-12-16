package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import java.util.List;

/**
 * @ClassName ReportFormTestProduceDTO
 * @Description 采购报表返回
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("采购报表返回")
public class ReportFormBuyTotalDTO {


    @ApiModelProperty("报表数据")
    private List<ReportFormBuyDTO> data;

    @ApiModelProperty("统计数据")
    private ReportFormBuySumDTO dataSum;

}
