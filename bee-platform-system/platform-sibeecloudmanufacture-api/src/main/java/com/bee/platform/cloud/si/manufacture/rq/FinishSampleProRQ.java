package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.util.Date;

/**
 * @author liang.li
 * @ClassName FinishSampleBuyRQ
 * @Description 采购完成取样RQ
 * @Date 2019/9/24
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "生产完成取样RQ")
public class FinishSampleProRQ {

    @ApiModelProperty(value = "炉号id")
    @NotNull(message = "炉号id不能为空")
    private Integer furnaceId;

    @ApiModelProperty(value = "班次:1一班，2二班，3三班")
    @NotNull(message = "班次不能为空")
    private Integer shift;

    @ApiModelProperty(value = "出炉批次")
    private Integer bakedBatch;

    @ApiModelProperty(value = "推送时间")
    @NotNull(message = "推送时间不能为空")
    private Date samplePushTime;
}
