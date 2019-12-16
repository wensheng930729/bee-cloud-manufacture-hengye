package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: BalanceStorageDTO
 * @Description: 一键平库实体类
 * @Author: fei.sun
 * @Date: 2019/9/29 10:24
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("一键平库实体类")
public class BalanceStorageDTO {

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("平库数量")
    private BigDecimal amount;

    @ApiModelProperty("产品单位")
    private String productUnit;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

}
