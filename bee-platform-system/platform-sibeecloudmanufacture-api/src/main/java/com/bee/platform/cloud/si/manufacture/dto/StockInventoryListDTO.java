package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @description: 盘点单列表返回数据
 * @author: junyang.li
 * @create: 2019-11-27 14:27
 **/
@Data
@Accessors(chain = true)
@ApiModel("盘点单列表返回数据")
public class StockInventoryListDTO implements Serializable {

    private static final long serialVersionUID = 1247967562083932813L;

    @ApiModelProperty("盘点单编号")
    private String inventoryOrderId;

    @ApiModelProperty("盘点日志")
    private Date createTime;

    public StockInventoryListDTO() {
    }

    public StockInventoryListDTO(String inventoryOrderId, Date createTime) {
        this.inventoryOrderId = inventoryOrderId;
        this.createTime = createTime;
    }
}
