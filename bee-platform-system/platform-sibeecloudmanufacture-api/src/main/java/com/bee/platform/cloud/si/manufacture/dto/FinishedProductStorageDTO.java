package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: FinishedProductStorageDTO
 * @Description: 产成品入库提交实体类
 * @Author: fei.sun
 * @Date: 2019/9/25 13:56
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("产成品入库提交实体类")
public class FinishedProductStorageDTO {

    @ApiModelProperty("业务唯一标识id")
    private String finishedProductPendingStorageId;

    @ApiModelProperty("吨袋编号")
    private String tonBagNumber;

    @ApiModelProperty("吨袋重量")
    private BigDecimal productNumber;

    @ApiModelProperty("货物id")
    private Integer productId;

    @ApiModelProperty("样品名称")
    private String productName;

    @ApiModelProperty("货物数量单位")
    private String productUnit;
}
