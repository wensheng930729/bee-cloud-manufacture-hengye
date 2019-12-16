package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("采购磅单号下拉列表DTO")
public class BuyWeightMachineBoxDTO {
	
    @ApiModelProperty("磅单id")
    private Integer id ;

    @ApiModelProperty("磅单业务id")
    private String machineId;
    
}
