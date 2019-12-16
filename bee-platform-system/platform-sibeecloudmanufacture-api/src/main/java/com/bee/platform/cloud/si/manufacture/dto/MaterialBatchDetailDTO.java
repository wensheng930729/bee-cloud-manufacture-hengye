package com.bee.platform.cloud.si.manufacture.dto;

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
@ApiModel(value = "料批明细返回参数")
public class MaterialBatchDetailDTO implements Serializable {
    private static final long serialVersionUID = -6096154235474958735L;

    @ApiModelProperty("id")
    private Long id;

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

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("质量要求")
    private String qualityRequirements;

    @ApiModelProperty("仓库id")
    private Integer warehouseId;

    @ApiModelProperty("仓库名称")
    private String warehouseName;

    @ApiModelProperty("plc出料斗id")
    private Integer plcFieldId;

    @ApiModelProperty("plc出料斗英文名称")
    private String plcField;

    @ApiModelProperty("plc出料斗中文名")
    private String plcFieldName;
}
