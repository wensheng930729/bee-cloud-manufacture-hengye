package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

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
@ApiModel("化验属性修改请求参数")
public class ConfigTestAttributeUpdateRQ implements Serializable{

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;
    /**
     * 化验属性名称
     */
    @ApiModelProperty("化验属性名称")
    @NotEmpty(message = "化验属性名称不能为空")
    private String attributeName;
    /**
     * 化验类型（0 输入项  1 输出项）
     */
    @ApiModelProperty("化验类型（0 输入项  1 输出项）")
    @NotNull(message = "化验类型不能为空")
    private Integer type;


}
