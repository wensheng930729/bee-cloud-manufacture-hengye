package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;

/**
 * @author liang.li
 * @ClassName FinishSampleBuyRQ
 * @Description 采购完成取样RQ
 * @Date 2019/9/24
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "采购完成取样RQ")
public class FinishSampleBuyRQ {

    @ApiModelProperty(value = "磅单业务id")
    @NotBlank(message = "磅单业务id不能为空")
    private String machineId;

}
