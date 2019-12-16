package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;
import java.math.BigDecimal;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(value = "出入厂展示数据")
public class EnterLeaveFactoryDTO implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@ApiModelProperty(value = "产品id")
    private Integer productId;

	@ApiModelProperty(value = "产品名称")
    private String productName;
    
	@ApiModelProperty(value = "净重")
    private BigDecimal netWeight;
	
}
