package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName: SampleQualityTestDTO
 * @Description: 销售出库质检信息
 * @Author: fei.sun
 * @Date: 2019/10/6 14:34
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售出库质检信息")
public class SampleQualityTestDTO {

    @ApiModelProperty("样品编号")
    private String sampleCode;

    @ApiModelProperty("化检时间")
    private String assayTime;

    @ApiModelProperty("质检结果")
    private List<AssayItem> qualityTestResult;


}
