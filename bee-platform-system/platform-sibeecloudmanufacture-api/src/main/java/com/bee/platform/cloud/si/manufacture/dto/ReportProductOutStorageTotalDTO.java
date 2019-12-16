package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 成品出库表
 * @Date 2019/10/23 14:06
 */
@Data
@Accessors(chain = true)
@ApiModel("成品出库表统计返回")
public class ReportProductOutStorageTotalDTO {

    @ApiModelProperty("出库数量")
    private BigDecimal outStorageNumber;


}
