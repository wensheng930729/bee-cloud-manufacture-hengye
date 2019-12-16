package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: RevisionStorageDTO
 * @Description: 盘库参数实体
 * @Author: fei.sun
 * @Date: 2019/9/28 16:02
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("盘库参数实体")
public class RevisionStorageDTO {


    @ApiModelProperty("货物id")
    private Integer productId;

    @ApiModelProperty("货物名称")
    private String productName;

    @ApiModelProperty("实际数量")
    private BigDecimal reviseProductNumber;

    @ApiModelProperty("当前数量")
    private BigDecimal currentProductNumber;

    @ApiModelProperty("货物单位")
    private String productUnit;

    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("盘库原因")
    private String reason;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;
}
