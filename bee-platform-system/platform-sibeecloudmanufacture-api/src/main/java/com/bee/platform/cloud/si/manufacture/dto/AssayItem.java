package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: AssayItem
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/8 9:19
 * @Version: 1.0
 */
@ApiModel("出库详情质检项")
@Data
@Accessors(chain = true)
public class AssayItem {

    @ApiModelProperty("化验项")
    private String assayItem;

    @ApiModelProperty("化验结果")
    private Double assayValue;

    @ApiModelProperty("化验结果单位")
    private String assayValueUnit;

}
