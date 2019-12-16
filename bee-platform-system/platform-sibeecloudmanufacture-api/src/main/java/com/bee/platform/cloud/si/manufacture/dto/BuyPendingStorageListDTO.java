package com.bee.platform.cloud.si.manufacture.dto;

import java.math.BigDecimal;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@ApiModel("web采购待入库列表")
@Accessors(chain = true)
public class BuyPendingStorageListDTO {

	@ApiModelProperty("入库产品业务Id")
    private String buyProductPendingStorageId;

    @ApiModelProperty("合同编号")
    private String contractId;
    
    @ApiModelProperty("供应商")
    private Integer supplierId;
    
    @ApiModelProperty("供应商")
    private String supplierName;

    @ApiModelProperty("车牌号")
    private String licensePlateNumber;

    @ApiModelProperty("产品数量")
    private BigDecimal productNumber;

    @ApiModelProperty("到厂时间")
    private String arrivalTime;
}
