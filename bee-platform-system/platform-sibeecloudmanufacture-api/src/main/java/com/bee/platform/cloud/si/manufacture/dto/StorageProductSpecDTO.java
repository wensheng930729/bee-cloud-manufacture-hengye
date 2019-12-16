package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: ProductSpecDTO
 * @Description: 销售出库产品规格
 * @Author: fei.sun
 * @Date: 2019/11/18 9:23
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售出库产品规格")
public class StorageProductSpecDTO {

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

}
