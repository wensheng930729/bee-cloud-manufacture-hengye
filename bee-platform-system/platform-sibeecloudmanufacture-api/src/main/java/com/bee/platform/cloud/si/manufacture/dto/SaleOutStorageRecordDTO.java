package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: SaleOutStorageRecordDTO
 * @Description: 销售出库记录
 * @Author: fei.sun
 * @Date: 2019/9/28 10:48
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售出库记录实体类")
public class SaleOutStorageRecordDTO {

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("购买方")
    private String purchaser;

    @ApiModelProperty("仓库Id")
    private String storageId;

    @ApiModelProperty("仓库名")
    private String storageName;

    @ApiModelProperty("出库量")
    private BigDecimal totalQuantity;

    @ApiModelProperty("吨袋数量")
    private Integer tonBagQuantity;

    @ApiModelProperty("出库人")
    private String operator;

    @ApiModelProperty("出库时间段")
    private String time;

}
