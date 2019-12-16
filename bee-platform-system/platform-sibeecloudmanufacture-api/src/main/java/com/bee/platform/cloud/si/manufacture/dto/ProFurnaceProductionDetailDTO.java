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
 * @date 2019/10/21
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "矿热炉产量数据明细返回信息")
public class ProFurnaceProductionDetailDTO implements Serializable {
    private static final long serialVersionUID = -3539745335089066361L;

    @ApiModelProperty(value = "炉号id")
    private Integer furnaceId;

    @ApiModelProperty(value = "炉号名称")
    private String furnaceName;

    @ApiModelProperty(value = "产品id")
    private Integer productId;

    @ApiModelProperty(value = "产品名称")
    private String productName;

    @ApiModelProperty(value = "产量")
    private BigDecimal amount;

    @ApiModelProperty(value = "当前日期")
    private String currentDate;

    @ApiModelProperty(value = "产品规格id")
    private Integer productSpecId;

    @ApiModelProperty(value = "产品规格名称")
    private String productSpecName;

    @ApiModelProperty(value = "料批吨耗")
    private BigDecimal tonConsume;

}
