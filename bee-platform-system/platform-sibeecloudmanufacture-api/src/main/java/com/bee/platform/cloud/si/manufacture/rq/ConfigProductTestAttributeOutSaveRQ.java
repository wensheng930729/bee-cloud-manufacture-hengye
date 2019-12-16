package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品化验结果项
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-24
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("保存产品化验结果项请求参数")
public class ConfigProductTestAttributeOutSaveRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    private Integer productId;
    /**
     * 化验结果项
     */
    @ApiModelProperty("化验结果项")
    private String assayItemOut;
    /**
     * 结果项标识符
     */
    @ApiModelProperty("结果项标识符")
    private String markOut;
    /**
     * 化验公式
     */
    @ApiModelProperty("化验公式")
    private String assayFormula;

    /**
     * 化验单位（0 %百分比  1 ‱万分比）
     */
    @ApiModelProperty("化验单位（0 %百分比  1 ‱万分比）")
    private Integer testUnit;

    /**
     * 小数位数（0整数 1 一位小数 2 二位小数 3 三位小数 4 四位小数 5 五位小数）
     */
    @ApiModelProperty("小数位数（0整数 1 一位小数 2 二位小数 3 三位小数 4 四位小数 5 五位小数）")
    private Integer decimalDigit;

}
