package com.bee.platform.cloud.si.manufacture.dto;

import com.bee.platform.cloud.si.manufacture.rq.CommonFileRq;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅数据信息保存入参
 * @Date 2019/9/23 19:23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "地磅数据总览信息入参")
public class WeightMachineTotalDTO implements Serializable {

    @ApiModelProperty(value = "合同编号")
    private String contractNum;

    @ApiModelProperty(value = "合同重量")
    private BigDecimal contractWeight;

    @ApiModelProperty(value = "已称重数量")
    private BigDecimal alreadyWeightAmount;

    @ApiModelProperty(value = "已称重车数")
    private Integer alreadyWeightCars;


}
