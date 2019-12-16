package com.bee.platform.cloud.si.manufacture.dto;

import cn.afterturn.easypoi.excel.annotation.Excel;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel("期初库存明细表返回信息")
public class ConfigOpeningInventoryOrderDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @ApiModelProperty("工厂id")
    private Integer factoryId;
    /**
     * 期初库存表id
     */
    @ApiModelProperty("期初库存表id")
    private Integer openingInventoryOrderId;
    /**
     * 产品_id
     */
    @ApiModelProperty("产品_id")
    private Integer productId;
    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String productName;

    /**
     * 产品规格id
     */
    @ApiModelProperty("产品规格id")
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    @ApiModelProperty("产品规格名称")
    private String productSpecName;
    /**
     * 仓库id
     */
    @ApiModelProperty("id")
    private Integer repositoryId;
    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    private String repositoryName;
    /**
     * 计量单位
     */
    @ApiModelProperty("计量单位")
    private String unit;
    /**
     * 化验结果
     */
    @ApiModelProperty("化验结果")
    private String testResult;
    /**
     * 期初数量
     */
    @ApiModelProperty("期初数量")
    private BigDecimal quantity;
    /**
     * 期初单价
     */
    @ApiModelProperty("期初单价")
    private BigDecimal price;
    /**
     * 期初金额
     */
    @ApiModelProperty("期初金额")
    private BigDecimal money;




}
