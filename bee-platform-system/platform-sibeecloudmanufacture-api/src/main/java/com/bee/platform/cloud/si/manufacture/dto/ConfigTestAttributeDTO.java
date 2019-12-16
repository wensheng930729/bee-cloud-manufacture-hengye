package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 化验属性配置表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("化验属性返回信息")
public class ConfigTestAttributeDTO implements Serializable{

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
     * 化验属性名称
     */
    @ApiModelProperty("化验属性名称")
    private String attributeName;
    /**
     * 化验类型（0 输入项  1 输出项）
     */
    @ApiModelProperty("化验类型（0 输入项  1 输出项）")
    private Integer type;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;
}
