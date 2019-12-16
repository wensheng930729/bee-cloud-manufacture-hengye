package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(value = "电流数据")
public class CurrentDTO implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
    @ApiModelProperty(value = "时间戳")
    private Long readTime;
	
    @ApiModelProperty(value = "推送时间")
    private String meterReadTime;
    
    @ApiModelProperty("数据")
    private Float iData;
    
    @ApiModelProperty("电流相-分别代表a,b,c")
    private String item;

}
