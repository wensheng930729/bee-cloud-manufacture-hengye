package com.bee.platform.cloud.si.manufacture.dto;

import java.util.Date;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("车次及样品信息")
public class BuyCarSampleDTO {

	@ApiModelProperty("磅单id")
    private String machineId;
	
	@ApiModelProperty("车牌号")
    private String trainNumber;
	
	@ApiModelProperty("司机")
    private String driver;
    
	@ApiModelProperty("称重时间")
    private Date weighingTime;
    
	@ApiModelProperty("质检结果")
    private Integer assayResult;
	
    @ApiModelProperty("再次化验按钮是否显示")
    private Integer rebackBtn;
}
