package com.bee.platform.cloud.si.manufacture.dto;

import java.math.BigDecimal;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("产成品待/已入库信息")
public class FinishedProductPendingStorageListDTO {
	
	@ApiModelProperty("业务唯一标识id")
    private String finishedProductPendingStorageId;

    @ApiModelProperty("吨袋编号")
    private String tonBagNumber;

    @ApiModelProperty("吨袋重量")
    private BigDecimal productNumber;

    @ApiModelProperty("货物id")
    private Integer productId;

    @ApiModelProperty("样品名称")
    private String productName;

    @ApiModelProperty("货物数量单位")
    private String productUnit;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("炉号")
    private String furnaceNumber;

    @ApiModelProperty("炉次")
    private String furnaceTimes;

    @ApiModelProperty("班次")
    private String scheduling;
    
    @ApiModelProperty("班次日期")
    @JsonFormat(pattern = "yyyy年M月d日")
    private Date shiftTime;
    
    @ApiModelProperty("入库日期")
    @JsonFormat(pattern = "yyyy年M月d日")
    private Date storageTime;
    
    

}
