package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;


@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "成品装袋详细信息出参")
public class ProBaggingDeatilsDTO extends ProBaggingDTO {


    @ApiModelProperty("样品编号（磅单取样标准的样品编号）")
    private String sampleCode;

    @ApiModelProperty("质检结果")
    private List<AssayItem> assayItemList;

    @ApiModelProperty("吨袋信息")
    private List<ProBaggingInfo> proBaggingInfos;
}
