package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: SaleOutStorageTonBagDTO
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/6 14:27
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("销售出库吨袋信息")
public class SaleOutStorageTonBagDTO {

    @ApiModelProperty("吨袋编号")
    private String tonBagNumber;

    @ApiModelProperty("吨袋重量")
    private BigDecimal productNumber;

    @ApiModelProperty("吨袋重量单位")
    private String productUnit;

    @ApiModelProperty("仓库名称")
    private String storageName;
}
