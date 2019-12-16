package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @author jie.zhang
 * @version 1.0.0
 * @ClassName ReportFormTestProduceDTO
 * @Description 采购报表返回
 */
@Data
@Accessors(chain = true)
@ApiModel("现存明细返回")
public class ReportFormExistingDeatilsDTO {

    @ApiModelProperty("产品")
    private String productName;

    @ApiModelProperty("规格")
    private String productSpecName;

    @ApiModelProperty("仓库")
    private String storageName;

    @ApiModelProperty("更新时间")
    private String updateTime;

    @ApiModelProperty("数量")
    private BigDecimal amount;
}
