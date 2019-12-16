package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;
import java.math.BigDecimal;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("看板-未完成业务账款情况")
public class UnfinishedGoodsDTO implements Serializable{

	private static final long serialVersionUID = 1L;

	@ApiModelProperty("账务项目id")
    private String itemId;
	
	@ApiModelProperty("账务项目")
    private String item;
	
	@ApiModelProperty(value = "业务已")
	private BigDecimal already;
	
	@ApiModelProperty(value = "业务未")
	private BigDecimal incomplete;
}
