package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author xin.huang
 * @description
 * @date 2019/11/12
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "企业电子大屏产量展示返回数据")
public class BigScreenProductionDTO implements Serializable {
    private static final long serialVersionUID = -7968639785856674710L;

    @ApiModelProperty(value = "炉号id")
    private Integer furnaceId;

    @ApiModelProperty(value = "炉号名称")
    private String furnaceName;

    @ApiModelProperty(value = "产量")
    private BigDecimal production;

    @ApiModelProperty(value = "产量占比")
    private BigDecimal proportion;

    @ApiModelProperty(value = "产品id")
    private Integer productId;

    @ApiModelProperty(value = "产品名称")
    private String productName;

    @ApiModelProperty(value = "产品规格id")
    private Integer productSpecId;

    @ApiModelProperty(value = "产品规格名称")
    private String productSpecName;
}
