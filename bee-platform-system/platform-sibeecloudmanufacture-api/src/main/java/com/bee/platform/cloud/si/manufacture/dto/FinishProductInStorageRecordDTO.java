package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: FinishProductInStorageRecordDTO
 * @Description: 产成品入库列表实体
 * @Author: fei.sun
 * @Date: 2019/9/30 14:14
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("产成品入库列表实体")
public class FinishProductInStorageRecordDTO {

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

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

    @ApiModelProperty("仓库id")
    private String storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("入库时间")
    private String time;

    @ApiModelProperty("入库数量")
    private BigDecimal productNumber;
}
