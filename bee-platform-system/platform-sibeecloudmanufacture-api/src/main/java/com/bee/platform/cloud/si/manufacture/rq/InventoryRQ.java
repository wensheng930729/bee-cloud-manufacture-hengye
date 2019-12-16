package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @description: 新增盘点单相关参数
 * @author: junyang.li
 * @create: 2019-11-26 13:22
 **/
@Data
@Accessors(chain = true)
@ApiModel("新增盘点单相关参数")
public class InventoryRQ implements Serializable {

    private static final long serialVersionUID = -3586422898494057964L;

    @ApiModelProperty("库存盘点类型的编号")
    @NotNull(message = "盘点分类不能为空")
    private Integer inventoryTypeCode;

    @ApiModelProperty("分类后具体类别的业务id")
    private List<Integer> categoryId;

}
