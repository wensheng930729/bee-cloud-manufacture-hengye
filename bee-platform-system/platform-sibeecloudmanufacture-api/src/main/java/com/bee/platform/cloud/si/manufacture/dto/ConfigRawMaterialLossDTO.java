package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 原料损耗配置表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("原料损耗返回信息")
public class ConfigRawMaterialLossDTO implements Serializable {

    private static final long serialVersionUID = 1L;
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 所属企业id
     */
    @ApiModelProperty("所属企业id")
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @ApiModelProperty("工厂id")
    private Integer factoryId;
    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    private Integer productId;
    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    private String productName;
    /**
     * 损耗
     */
    @ApiModelProperty("损耗")
    private BigDecimal loss;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;

}
