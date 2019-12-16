package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("样品及样品结果详情")
public class CarRemarkDTO implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
    @ApiModelProperty("车牌号")
    private String trainNumber;
    
    @ApiModelProperty("磅单备注")
    private String remark;
}
