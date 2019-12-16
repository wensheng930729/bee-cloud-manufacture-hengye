package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: BuyInStorageRecordDTO
 * @Description: 采购入库记录实体类
 * @Author: fei.sun
 * @Date: 2019/9/30 14:04
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("采购入库记录实体类")
public class BuyInStorageRecordDTO {

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("入库数量")
    private BigDecimal productNumber;

    @ApiModelProperty("入库时间段")
    private String time;
}
