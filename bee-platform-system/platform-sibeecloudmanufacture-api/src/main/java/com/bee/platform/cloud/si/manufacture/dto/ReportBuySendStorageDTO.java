package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @author jie.zhang
 * @version 1.0.0
 * @ClassName ReportFormTestProduceDTO
 * @Description 采购报表返回
 */
@Data
@Accessors(chain = true)
@ApiModel("采购入库返回")
public class ReportBuySendStorageDTO {

    @ApiModelProperty("合同号")
    private String contractNum;

    @ApiModelProperty("合同签订日期")
    private String contractSignDate;

    @ApiModelProperty("入库时间")
    private String storageTime;

    @ApiModelProperty("产品")
    private String productName;

    @ApiModelProperty("规格")
    private String productSpecName;

    @ApiModelProperty("仓库")
    private String storageName;

    @ApiModelProperty("入库数量")
    private BigDecimal sendStorageAmount;

//    @ApiModelProperty("化验项")
//    private List<ReportFormTestQualityTestAssayDTO> items;
    @ApiModelProperty("化验项")
    private List<Map<String,Object>> items;
}
