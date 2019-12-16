package com.bee.platform.cloud.si.manufacture.dto;

import java.util.List;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("采购样品信息")
public class BuySampleInfoDTO {
	
    @ApiModelProperty("统计个数")
    private Integer counts;

    @ApiModelProperty("样品列表")
    private List<SampleResultDTO> sampleResultDTOs;
    
}
