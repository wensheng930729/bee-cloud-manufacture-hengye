package com.bee.platform.cloud.si.manufacture.rq;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "车次货物样品确认请求参数")
public class BuyCarSampleSureRQ implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
    @ApiModelProperty("磅单id")
    @NotNull(message="磅单id不能为空")
    private String machineId;
    
    @ApiModelProperty("规格id")
    @NotNull(message="规格id不能为空")
    private Integer productSpecId;
    
    @ApiModelProperty("规格名称")
    @NotNull(message="规格名称不能为空")
    private String productSpecName;
    
    @ApiModelProperty("质检扣重")
    private BigDecimal carDeductWeight;
    
    @ApiModelProperty("质检主任化验结果0不合格 1合格")
    private Integer assayResult;

}
