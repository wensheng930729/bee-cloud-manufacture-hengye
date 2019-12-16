package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品化验输入项
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-24
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品化验输入项返回信息")
public class ConfigProductTestAttributeInDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;

    /**
     * 产品id
     */
    @ApiModelProperty("id")
    private Integer productId;
    /**
     * 化验输入项
     */
    @ApiModelProperty("化验输入项")
    private String assayItemIn;
    /**
     * 输入项标识符
     */
    @ApiModelProperty("输入项标识符")
    private String markIn;

    /**
     * 化验单位（0 %百分比  1 ‱万分比）
     */
    @ApiModelProperty("化验单位（0 %百分比  1 ‱万分比）")
    private Integer testUnit;

    @ApiModelProperty("单位字符串标识")
    private String unitString;
}
