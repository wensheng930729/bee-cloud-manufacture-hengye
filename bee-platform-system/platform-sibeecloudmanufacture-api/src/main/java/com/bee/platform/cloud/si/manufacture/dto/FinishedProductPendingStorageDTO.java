package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: FinishedProductPendingStorageDTO
 * @Description: 保存产成品待入库参数实体类
 * @Author: fei.sun
 * @Date: 2019/9/25 10:51
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("保存和返回产成品待入库参数实体类")
public class FinishedProductPendingStorageDTO {


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

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("炉号")
    private String furnaceNumber;

    @ApiModelProperty("炉次")
    private String furnaceTimes;

    @ApiModelProperty("班次")
    private String scheduling;
}
