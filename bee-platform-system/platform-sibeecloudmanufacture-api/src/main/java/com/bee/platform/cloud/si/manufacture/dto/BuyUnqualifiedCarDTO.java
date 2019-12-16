package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(value = "不合格车辆信息")
public class BuyUnqualifiedCarDTO implements Serializable{

	private static final long serialVersionUID = 1L;
	
    @ApiModelProperty(value = "磅单id")
    private String machineId;

	@ApiModelProperty(value = "车牌号")
    private String trainNumber;
    
    @ApiModelProperty(value = "批次ID")
    private String batchId;
    
    @ApiModelProperty(value = "批次名称")
    private String batchName;
    
    @ApiModelProperty(value = "净重")
    private BigDecimal netWeight;
    
}
