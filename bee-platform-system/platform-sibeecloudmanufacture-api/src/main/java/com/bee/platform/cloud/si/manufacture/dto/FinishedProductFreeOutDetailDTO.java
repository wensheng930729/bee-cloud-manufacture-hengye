package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: FinishedProductFreeOutDetailDTO
 * @Description: 销售出库新增出库
 * @Author: fei.sun
 * @Date: 2019/11/18 17:47
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售出库新增出库")
public class FinishedProductFreeOutDetailDTO {

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("产品数量")
    private BigDecimal productNumber;

    @ApiModelProperty("仓库名称")
    private String storageName;
}
