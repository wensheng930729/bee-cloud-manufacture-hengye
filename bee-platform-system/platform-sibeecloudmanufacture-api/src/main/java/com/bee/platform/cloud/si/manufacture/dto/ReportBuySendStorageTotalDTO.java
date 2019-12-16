package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @author jie.zhang
 * @version 1.0.0
 * @ClassName ReportFormTestProduceDTO
 * @Description 采购报表返回
 */
@Data
@Accessors(chain = true)
@ApiModel("采购入库统计返回")
public class ReportBuySendStorageTotalDTO {

    @ApiModelProperty("入库数量")
    private BigDecimal sendStorageAmount;

}
