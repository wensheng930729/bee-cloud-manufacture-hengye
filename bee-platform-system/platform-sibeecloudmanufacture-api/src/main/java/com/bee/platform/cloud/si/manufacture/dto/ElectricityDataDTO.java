package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(value = "电流变化展示数据")
public class ElectricityDataDTO implements Serializable {

	private static final long serialVersionUID = 1L;
	
    @ApiModelProperty(value = "推送时间")
    private String meterReadTime;
    
    @ApiModelProperty("a数据")
    private Float ia;
    
    @ApiModelProperty("b数据")
    private Float ib;
    
    @ApiModelProperty("c数据")
    private Float ic;

}
