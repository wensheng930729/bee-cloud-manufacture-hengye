package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName ReportFormProductWarehouseSumDTO
 * @Description 产成品入库报表合计返回
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("产成品入库报表合计返回")
public class ReportFormProductWarehouseSumDTO {

    @ApiModelProperty("入库数量合计")
    private BigDecimal tonWeight;

}
