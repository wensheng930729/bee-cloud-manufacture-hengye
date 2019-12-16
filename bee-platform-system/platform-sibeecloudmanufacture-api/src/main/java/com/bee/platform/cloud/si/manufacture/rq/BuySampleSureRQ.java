package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

import javax.validation.constraints.NotNull;


@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "确认样品检验结果请求参数")
public class BuySampleSureRQ implements Serializable {

    private static final long serialVersionUID = -1478450944062052089L;

    @ApiModelProperty("样品编号")
    @NotNull(message="样品编号不能为空")
    private String sampleCode;

    @ApiModelProperty("质检主任化验结果0不合格 1合格")
    private Integer assayResult;
}
