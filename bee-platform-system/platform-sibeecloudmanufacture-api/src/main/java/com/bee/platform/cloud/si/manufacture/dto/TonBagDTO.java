package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName: TonBagDTO
 * @Description: 待出库吨袋信息
 * @Author: fei.sun
 * @Date: 2019/9/26 19:58
 * @Version: 1.0
 */
@Data
@ApiModel("待出库吨袋信息")
@Accessors(chain = true)
public class TonBagDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("吨袋编号")
    private String tonBagNumber;

    @ApiModelProperty("吨袋重量")
    private BigDecimal productNumber;

    @ApiModelProperty("吨袋重量单位")
    private String productUnit;
}
