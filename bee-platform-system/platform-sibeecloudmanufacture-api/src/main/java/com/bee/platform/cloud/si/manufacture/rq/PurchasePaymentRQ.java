package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;


/**
 * @author liliang
 * @version 1.0.0
 * @Description 采购付款情况rq
 * @Date 2019-10-22
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "采购付款情况rq")
public class PurchasePaymentRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty(value = "1主料2辅料3成品4采购商/供应商")
    @NotNull
    private Integer type;

    @ApiModelProperty(value = "开始时间")
    @NotBlank
    private String startTime;

    @ApiModelProperty(value = "结束时间")
    @NotBlank
    private String endTime;


}
