package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: PickOutProductDTO
 * @Description: 领用出库参数实体类
 * @Author: fei.sun
 * @Date: 2019/9/27 16:16
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("领用出库参数实体类")
public class PickOutProductDTO {

    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("领用物id")
    private Integer productId;

    @ApiModelProperty("领用数量")
    private BigDecimal productNumber;

    @ApiModelProperty("领用人")
    private String receiver;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;
}
