package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: FinishedProductOutDTO
 * @Description: 手输成品出库实体类
 * @Author: fei.sun
 * @Date: 2019/11/15 16:00
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("手输成品出库实体类")
public class FinishedProductOutDTO {

    @ApiModelProperty("待出库车辆唯一标识id")
    private String contractCarId;

    @ApiModelProperty("成品id")
    private Integer productId;

    @ApiModelProperty("成品名称")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("产品数量")
    private BigDecimal productNumber;

    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;
}
