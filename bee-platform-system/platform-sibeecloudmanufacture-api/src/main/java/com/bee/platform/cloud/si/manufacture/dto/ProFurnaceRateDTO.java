package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author xin.huang
 * @description 矿热炉回收率返回信息
 * @date 2019/10/18
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "矿热炉回收率返回信息")
public class ProFurnaceRateDTO implements Serializable {
    private static final long serialVersionUID = 7008715440778512352L;

    @ApiModelProperty(value = "炉号id")
    private Integer furnaceId;

    @ApiModelProperty(value = "炉号名称")
    private String furnaceName;

    @ApiModelProperty(value = "回收率")
    private BigDecimal recoveryRate;
}
