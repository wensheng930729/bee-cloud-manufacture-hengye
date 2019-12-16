package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;


@Data
@Accessors(chain = true)
@ApiModel(value = "Web地磅数据信息头部统计出参")
public class WeightMachineWebCountDTO implements Serializable {

    @ApiModelProperty(value = "当前列表待称重车辆数")
    private Integer waitWeightCar;

    @ApiModelProperty(value = "当前列表已称重车辆数")
    private Integer alreadyWeightCar;

    @ApiModelProperty(value = "当前列表已称重净重")
    private BigDecimal alreadyNetWeight;

    public WeightMachineWebCountDTO(Integer waitWeightCar,Integer alreadyWeightCar,BigDecimal alreadyNetWeight){
        this.waitWeightCar = waitWeightCar;
        this.alreadyWeightCar = alreadyWeightCar;
        this.alreadyNetWeight = alreadyNetWeight;
    }
}
