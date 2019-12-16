package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/26
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "配电记录明细返回信息")
public class ProPowerRecordDetailDTO implements Serializable {

    private static final long serialVersionUID = 582932999371598072L;

    @ApiModelProperty(value = "时间")
    private String time;

    @ApiModelProperty(value = "炉变档位")
    private String gear;

    @ApiModelProperty(value = "炉变油温")
    private String oilTemperature;

    @ApiModelProperty(value = "补偿设备：1开，0关")
    private Integer device;
}
