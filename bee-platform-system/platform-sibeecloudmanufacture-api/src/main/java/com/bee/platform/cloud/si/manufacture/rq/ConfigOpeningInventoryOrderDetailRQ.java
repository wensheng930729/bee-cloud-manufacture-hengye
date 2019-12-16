package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 期初库存明细表
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("保存期初库存明细请求参数")
public class ConfigOpeningInventoryOrderDetailRQ implements Serializable {

    private static final long serialVersionUID = 1L;



    /**
     * 期初库存表id
     */
    @ApiModelProperty("期初库存表id")
    private Integer openingInventoryOrderId;
    /**
     * 产品_id
     */
    @ApiModelProperty("产品_id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;
    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    @NotEmpty(message = "产品名称不能为空")
    private String productName;

    /**
     * 产品规格id
     */
    @ApiModelProperty("产品规格id")
    @NotNull(message = "产品规格id不能为空")
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    @ApiModelProperty("产品规格名称")
    @NotEmpty(message = "产品规格名称不能为空")
    private String productSpecName;
    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库id不能为空")
    private Integer repositoryId;
    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    @NotEmpty(message = "仓库名称不能为空")
    private String repositoryName;
    /**
     * 计量单位
     */
    @ApiModelProperty("计量单位")
    @NotEmpty(message = "计量单位不能为空")
    private String unit;
    /**
     * 化验结果
     */
    @ApiModelProperty("化验结果")
    @NotEmpty(message = "化验结果不能为空")
    private String testResult;
    /**
     * 期初数量
     */
    @ApiModelProperty("期初数量")
    @NotNull(message = "期初数量不能为空")
    private BigDecimal quantity;
    /**
     * 期初单价
     */
    @ApiModelProperty("期初单价")
//    @NotNull(message = "期初单价不能为空")
    private BigDecimal price;
    /**
     * 期初金额
     */
    @ApiModelProperty("期初金额")
//    @NotNull(message = "期初金额不能为空")
    private BigDecimal money;




}
