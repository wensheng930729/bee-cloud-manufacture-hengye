package com.bee.platform.cloud.si.manufacture.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@ApiModel("web采购待入库实体详情")
@Accessors(chain = true)
public class BuyPendingStorageProductMsgDTO {
	
	/**********业务信息************/
	
	@ApiModelProperty("是否已入库（0待入库；1已入库）")
	private Integer putStorage;
	
	@ApiModelProperty("待入库产品业务Id")
    private String buyProductPendingStorageId;

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("车牌号")
    private String licensePlateNumber;
    
    @ApiModelProperty("处理方式(0折价入库，1确认入库)")
    private Integer processMode;
    
    /**********产品信息**********/
    
    @ApiModelProperty("产品Id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;
    
    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;
    
    @ApiModelProperty("化验结果(0不合格，1合格)")
    private Integer analysisResult;

    @ApiModelProperty("产品数量")
    private BigDecimal productNumber;

    @ApiModelProperty("产品单位")
    private String productUnit;

    @ApiModelProperty("到厂时间")
    private String arrivalTime;

    @ApiModelProperty("磅房备注")
    private String remark;
    
    /*****入库信息******/
    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("入库数量")
    private BigDecimal actualProductNumber;

    @ApiModelProperty("入库时间")
    private String storageTime;

}
