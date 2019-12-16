package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @ClassName: BulkPendingStorageProductDTO
 * @Description: 批量入库参数实体类
 * @Author: fei.sun
 * @Date: 2019/9/27 9:55
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("批量入库参数实体类")
public class BulkPendingStorageProductDTO {

    @ApiModelProperty("折价后单价")
    private BigDecimal discountedPrice;

    @ApiModelProperty("待入库车辆榜单id")
    private List<String> machineIds;

   /* @ApiModelProperty("待入库产品实体类")
    private List<ProductPendingStorageDTO> productPendingStorageDTOS;*/
}
