package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: RawMaterialDTO
 * @Description: 原料库存情况实体类
 * @Author: fei.sun
 * @Date: 2019/10/18 16:25
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("原料库存情况实体类")
public class RawMaterialDTO {

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品数量")
    private BigDecimal productNumber;
}
