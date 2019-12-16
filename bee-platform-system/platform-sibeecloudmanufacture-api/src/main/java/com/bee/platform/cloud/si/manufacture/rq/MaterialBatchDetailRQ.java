package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/24
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "料批明细新增参数")
public class MaterialBatchDetailRQ implements Serializable {
    private static final long serialVersionUID = -8862638639099268305L;

    @ApiModelProperty("料批主表信息id")
    private Long batchId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("产品数量")
    private BigDecimal num;

    @ApiModelProperty("产品单位")
    private String unit;

    @ApiModelProperty("质量要求")
    private String qualityRequirements;

    @ApiModelProperty("仓库id")
    private Integer warehouseId;

    @ApiModelProperty("仓库名称")
    private String warehouseName;

    @ApiModelProperty("plc出料斗field")
    private String plcField;

}
