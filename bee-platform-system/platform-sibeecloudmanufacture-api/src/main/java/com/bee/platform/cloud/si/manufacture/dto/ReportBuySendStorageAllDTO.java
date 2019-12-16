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
@ApiModel("采购入库总返回")
public class ReportBuySendStorageAllDTO {

    @ApiModelProperty("报表数据")
    private List<ReportBuySendStorageDTO> data;

    @ApiModelProperty("统计值")
    private ReportBuySendStorageTotalDTO dataSum;

}
