package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.hibernate.validator.constraints.Length;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-11-26 17:36
 **/
@Data
@ToString
@NoArgsConstructor
@ApiModel("保存盘点单的信息")
public class StockInventoryRQ implements Serializable {

    private static final long serialVersionUID = -6563828943959514222L;

    @ApiModelProperty("盘点单单号")
    @NotEmpty(message = "盘点单号不能为空")
    @Length(max = 15,message = "盘点单号限制15个字符")
    private String inventoryOrderId;

    @ApiModelProperty("盘点单名称，非必填")
    @Length(max = 25,message = "盘点单名称限制25个字符")
    private String inventoryName;

    @ApiModelProperty("备注")
    @Length(max = 40,message = "备注限制40个字符")
    private String remarks;

    @ApiModelProperty("产品库存信息")
    @NotEmpty(message = "产品库存信息不能为空")
    @Valid
    private List<StockInventoryDetailRQ> inventoryDetail;

}
