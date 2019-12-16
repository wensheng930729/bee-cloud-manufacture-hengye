package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: StorageManagerListDTO
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/9/28 13:47
 * @Version: 1.0
 */

@Data
@ApiModel("仓库管理列表实体类")
@Accessors(chain = true)
public class StorageManagerListDTO {

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品单位")
    private String productUnit;

    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("产品库存总量")
    private BigDecimal productAmount;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("无规格产品数量(只有在成品查询时才有这个字段)")
    private BigDecimal noSpecProductNum;


}
