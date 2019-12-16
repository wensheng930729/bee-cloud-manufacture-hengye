package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @description: 盘点单产品详细
 * @author: junyang.li
 * @create: 2019-11-26 14:00
 **/
@Data
@Accessors(chain = true)
@ApiModel("盘点单产品详细")
public class InventoryProductDetailDTO implements Serializable {

    private static final long serialVersionUID = -5497672947498311253L;

    /**
     * 库存表中的主键id，方便后面更新库存，无需返回给前端
     */
    private Long storageInventoryId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("产品计量单位")
    private String productUnit;

    @ApiModelProperty("账面数量")
    private BigDecimal accountNum;

    @ApiModelProperty("实盘数量")
    private BigDecimal actualNum;

    @ApiModelProperty("差异数量")
    private BigDecimal differenceNum;


    public InventoryProductDetailDTO() {
    }

    public InventoryProductDetailDTO(Long storageInventoryId,Integer productId, String productName,
                                     Integer productSpecId, String productSpecName) {
        this.storageInventoryId=storageInventoryId;
        this.productId = productId;
        this.productName = productName;
        this.productSpecId = productSpecId;
        this.productSpecName = productSpecName;
    }
}
