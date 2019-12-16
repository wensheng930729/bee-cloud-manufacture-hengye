package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;

/**
 * @author liang.li
 * @ClassName FinishSampleBuyRQ
 * @Description 销售完成取样RQ
 * @Date 2019/9/26
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "销售完成取样RQ")
public class FinishSampleSaleRQ {

    @ApiModelProperty(value = "合同业务id")
    @NotBlank(message = "合同业务id不能为空")
    private String contractBusinessId;

}
