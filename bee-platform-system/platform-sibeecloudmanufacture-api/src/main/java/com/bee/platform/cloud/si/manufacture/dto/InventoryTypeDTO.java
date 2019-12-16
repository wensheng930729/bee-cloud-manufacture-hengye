package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 盘点类型返回数据
 * @author: junyang.li
 * @create: 2019-11-25 17:00
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("库存盘点类型的返回数据")
public class InventoryTypeDTO implements Serializable {

    private static final long serialVersionUID = 1247579420702498672L;

    @ApiModelProperty("库存盘点类型的编号")
    private Integer inventoryTypeCode;

    @ApiModelProperty("库存盘点类型的描述")
    private String inventoryTypeDesc;

    public InventoryTypeDTO(Integer inventoryTypeCode, String inventoryTypeDesc) {
        this.inventoryTypeCode = inventoryTypeCode;
        this.inventoryTypeDesc = inventoryTypeDesc;
    }
}
