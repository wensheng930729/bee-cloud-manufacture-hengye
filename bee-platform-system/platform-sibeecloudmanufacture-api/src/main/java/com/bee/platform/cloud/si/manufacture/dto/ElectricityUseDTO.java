package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(value = "电量使用情况")
public class ElectricityUseDTO implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@ApiModelProperty("热炉号")
    private String deviceId;
    
    @ApiModelProperty(value = "日期")
    private String time;
    
    @ApiModelProperty(value = "电量")
    private Float power;

}
