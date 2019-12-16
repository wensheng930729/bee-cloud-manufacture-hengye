package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/24
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "料批新增（编辑）参数")
public class MaterialBatchRQ implements Serializable {
    private static final long serialVersionUID = -6569755365831755167L;

    @ApiModelProperty("料批id")
    private Long id;

    @ApiModelProperty("料批名称")
    private String materialName;

    @ApiModelProperty("成品id")
    private Integer finishProductId;

    @ApiModelProperty("成品名称")
    private String finishProductName;

    @ApiModelProperty("plc设备id")
    private Integer plcId;

    @ApiModelProperty("生产数量")
    private BigDecimal productionNum;

    @ApiModelProperty("成品单位")
    private String finishUnit;

    @ApiModelProperty("状态：1启用，0停用")
    private Integer active;

    @ApiModelProperty("料批明细新增参数")
    private List<MaterialBatchDetailRQ> materialBatchDetailList;
}
