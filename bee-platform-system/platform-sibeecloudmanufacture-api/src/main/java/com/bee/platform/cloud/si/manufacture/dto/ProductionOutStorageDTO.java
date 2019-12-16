package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: ProductionOutStorageDTO
 * @Description: 生产出库记录实体类
 * @Author: fei.sun
 * @Date: 2019/9/28 13:13
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("生产出库记录实体类")
public class ProductionOutStorageDTO {

    @ApiModelProperty("物品名称")
    private String productName;

    @ApiModelProperty("物品数量")
    private BigDecimal productNumber;

    @ApiModelProperty("物品单位")
    private String productUnit;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("出库人")
    private String outStoragePerson;

    @ApiModelProperty("出库原因")
    private String reason;

    @ApiModelProperty("出库时间")
    private String outStorageTime;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;
}
