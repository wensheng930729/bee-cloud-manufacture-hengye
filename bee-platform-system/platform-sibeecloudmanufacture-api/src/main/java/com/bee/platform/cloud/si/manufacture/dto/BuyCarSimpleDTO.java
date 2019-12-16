package com.bee.platform.cloud.si.manufacture.dto;

import java.math.BigDecimal;
import java.util.Date;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("车次信息")
public class BuyCarSimpleDTO {

	@ApiModelProperty("合同业务id")
    private String contractBusinessId;
    
	@ApiModelProperty("磅单id")
    private String machineId;
	
	@ApiModelProperty("车牌号")
    private String trainNumber;
	
	@ApiModelProperty("司机")
    private String driver;
    
	@ApiModelProperty("称重时间")
    private Date weighingTime;
    
	@ApiModelProperty("样品名称")
    private String productName;
	
	@ApiModelProperty("规格id")
    private Integer productSpecId;

	@ApiModelProperty("规格名称")
    private String productSpecName;
	
	@ApiModelProperty("质检扣重")
    private BigDecimal carDeductWeight;
	
	@ApiModelProperty("质检结果")
    private Integer assayResult;
	
	@ApiModelProperty("磅单备注")
    private String remark;
	
	@ApiModelProperty("联系方式")
    private String contact;
    
}
