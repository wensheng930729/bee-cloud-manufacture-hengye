package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @description: 盘点订单详细
 * @author: junyang.li
 * @create: 2019-11-26 13:30
 **/
@Data
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("盘点订单详细")
public class InventoryOrderDTO implements Serializable {

    private static final long serialVersionUID = 2431395987626611321L;

    @ApiModelProperty("盘点单单号")
    private String inventoryOrderId;

    @ApiModelProperty("盘点单名称")
    private String inventoryName;

    @ApiModelProperty("盘点分类 ")
    private Integer inventoryType;

    @ApiModelProperty("工厂id")
    private Integer factoryId;

    @ApiModelProperty("企业id")
    private Integer enterpriseId;

    @ApiModelProperty("备注")
    private String remarks;

    @ApiModelProperty("是否可更改")
    private Integer immutable;

    @ApiModelProperty("创建人id")
    private Integer createId;

    @ApiModelProperty("创建人姓名")
    private String creator;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("产品详细")
    private List<InventoryProductDetailDTO> list;
}
