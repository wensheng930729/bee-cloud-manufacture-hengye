package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品规格表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品规格返回信息")
public class ConfigProductSpecDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 企业id
     */
    @ApiModelProperty("企业id")
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
     * 规格名称
     */
    @ApiModelProperty("规格名称")
    private String specName;
    /**
     * 合格线（0 否 1 是）
     */
    @ApiModelProperty("合格线（0 否 1 是）")
    private Integer qualifiedLine;
    /**
     * 状态(1-启用 ,0-禁用)
     */
    @ApiModelProperty("状态(1-启用 ,0-禁用)")
    private Integer status;
    /**
     * 排序
     */
    @ApiModelProperty("排序")
    private Integer sort;





}
