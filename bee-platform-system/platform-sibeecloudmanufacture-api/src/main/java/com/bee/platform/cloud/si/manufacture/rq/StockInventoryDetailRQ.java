package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.Digits;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 库存盘点详细
 * </p>
 *
 * @author junyang.li123
 * @since 2019-11-25
 */
@Data
@Accessors(chain = true)
@ApiModel("盘点单产品库存信息")
public class StockInventoryDetailRQ implements Serializable{

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品编号不能为空")
    private Integer productId;

    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库编号不能为空")
    private Integer storageId;

    @ApiModelProperty("产品规格id")
    @NotNull(message = "产品规格不能为空")
    private Integer productSpecId;

    @ApiModelProperty("实盘数量")
    @Digits(integer = 7,fraction = 2,message = "实盘数量只能保留7位整数和2位小数")
    private BigDecimal actualNum;

}
