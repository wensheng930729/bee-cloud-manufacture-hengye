package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author liliang
 * @version 1.0.0
 * @ClassName SalePassRatioDTO
 * @Description BI销售合格率DTO
 * @Date 2019/10/16 9:31
 */
@Data
@Accessors(chain = true)
@ApiModel("BI销售合格率DTO")
public class SalePassRatioDTO {

    @ApiModelProperty("客户id")
    private Integer customerId;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("合格率")
    private BigDecimal passRatio;

    @ApiModelProperty("不合格率")
    private BigDecimal failureRatio;


}
