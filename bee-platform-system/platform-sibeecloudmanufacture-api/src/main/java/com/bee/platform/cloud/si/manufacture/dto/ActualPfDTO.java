package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(value = "实时总功率因子")
public class ActualPfDTO implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("热炉号")
    private String deviceId;
	
    @ApiModelProperty(value = "功率因素")
    private Float pf;
}
