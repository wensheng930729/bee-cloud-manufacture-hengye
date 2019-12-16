package com.bee.platform.cloud.si.manufacture.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(value = "采购车辆信息")
public class BuyCarDTO {

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
    
    @ApiModelProperty(value = "收货日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date weighingTime;
    
    @ApiModelProperty(value = "司磅员")
    private String weightMan;
    
    @ApiModelProperty(value = "质检结果")
    private Integer assayResult;
    
    @ApiModelProperty(value = "处理方式（0-折价入库，1-确认入库）")
    private Integer handleType;
    

}
