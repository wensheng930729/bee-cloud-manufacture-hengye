package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author xin.huang
 * @description 矿热炉动力电消耗返回信息
 * @date 2019/10/22
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "矿热炉动力电消耗返回信息")
public class ProFurnacePowerConsumeDTO implements Serializable {
    private static final long serialVersionUID = 264648881360194716L;

    @ApiModelProperty(value = "矿热炉id")
    private Integer furnaceId;

    @ApiModelProperty(value = "矿热炉名称")
    private String furnaceName;

    @ApiModelProperty(value = "动力电消耗")
    private BigDecimal powerConsume;

    @ApiModelProperty(value = "吨电耗")
    private BigDecimal tonConsume;

    @ApiModelProperty(value = "当前日期")
    private String currentDate;
}
