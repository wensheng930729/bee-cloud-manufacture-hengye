package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @ClassName ReportFormProductWarehouseTotalDTO
 * @Description 产成品入库报表总返回
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("产成品入库报表总返回")
public class ReportFormProductWarehouseTotalDTO {

    @ApiModelProperty("报表数据")
    private List<ReportFormProductWarehouseDTO> data;

    @ApiModelProperty("统计值")
    private ReportFormProductWarehouseSumDTO dataSum;

}
