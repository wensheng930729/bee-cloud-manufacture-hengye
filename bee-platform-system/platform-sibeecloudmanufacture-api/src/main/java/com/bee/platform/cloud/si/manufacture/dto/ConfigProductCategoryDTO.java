package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品类别
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品类别返回信息")
public class ConfigProductCategoryDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
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
     * 产品类别名称
     */
    @ApiModelProperty("产品类别名称")
    private String name;
    /**
     * 状态:1-启用,0-禁用
     */
    @ApiModelProperty("状态:1-启用,0-禁用")
    private Integer status;
    /**
     * 产品类别分类（0默认分类，1企业自定义分类）
     */
    @ApiModelProperty("产品类别分类（0默认分类，1企业自定义分类）")
    private Integer type;




}
