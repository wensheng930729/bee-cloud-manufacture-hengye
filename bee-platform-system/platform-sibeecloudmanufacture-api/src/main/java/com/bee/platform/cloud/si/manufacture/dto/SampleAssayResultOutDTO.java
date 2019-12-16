package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

@Data
@Accessors(chain = true)
@ApiModel("样品化验输出项")
public class SampleAssayResultOutDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("样品编码")
    private String sampleCode;

    @ApiModelProperty("化验项")
    private String assayItem;

    @ApiModelProperty("化验结果")
    private Double assayValue;

    @ApiModelProperty("类型0输入项1输出项")
    private Integer type;

    @ApiModelProperty("业务类型1采购2销售3生产")
    private Integer businessType;
    /**
     * 化验单位（0 %百分比  1 ‱万分比）
     */
    @ApiModelProperty("化验单位（0 %百分比  1 ‱万分比）")
    private Integer testUnit;

    @ApiModelProperty("单位字符串标识")
    private String unitString;

}
