package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: ProductionOutStorageDetailDTO
 * @Description: 新增生产出库实体类
 * @Author: fei.sun
 * @Date: 2019/9/27 17:36
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("新增生产出库实体类")
public class ProductionOutStorageDetailDTO {

    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("货物名称")
    private String productName;

    @ApiModelProperty("货物数量")
    private BigDecimal productNumber;

    @ApiModelProperty("货物单位")
    private String productUnit;

    @ApiModelProperty("出库原因")
    private String reason;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;
}
