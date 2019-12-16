package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("化验结果")
public class SaleBackAssayResultDTO implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("化验项")
    private String assayItem;

	@ApiModelProperty("化验结果")
    private Double assayValue;
	
    @ApiModelProperty("化验单位（0 %百分比  1 ‱万分比）")
    private Integer testUnit;
    
    @ApiModelProperty("单位字符串标识")
    private String unitString;

}
