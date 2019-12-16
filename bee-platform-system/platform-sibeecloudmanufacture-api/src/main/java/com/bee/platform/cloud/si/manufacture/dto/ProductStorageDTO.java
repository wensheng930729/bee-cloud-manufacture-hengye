package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.annotations.ApiParam;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: ProductPendingStorageDTO
 * @Description: 产品入库实体类
 * @Author: fei.sun
 * @Date: 2019/9/23 16:31
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("产品入库实体类")
public class ProductStorageDTO {
    /**
     * 待入库产品业务Id
     */
    @ApiModelProperty("待入库产品业务Id")
    private String buyProductPendingStorageId;
    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
    private Integer storageId;
    /**
     * 货物id
     */
    @ApiModelProperty("货物id")
    private Integer productId;
    /**
     * 货物名称
     */
    @ApiModelProperty("货物名称")
    private String productName;
    /**
     * 货物数量（入库）
     */
    @ApiModelProperty("货物数量")
    private BigDecimal productNumber;
    /**
     * 货物单位
     */
    @ApiModelProperty("货物单位")
    private String productUnit;
    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    private String storageName;
}
