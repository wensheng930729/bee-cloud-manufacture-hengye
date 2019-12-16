package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;

/**
 * @author liang.li
 * @ClassName PurchaserReAssayBuyRQ
 * @Description 采购采购商重新化验请求rq
 * @Date 2019/9/25
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "采购采购商重新化验请求rq")
public class PurchaserReAssayBuyRQ {

    @ApiModelProperty(value = "磅单id")
    @NotBlank(message = "磅单id不能为空")
    private String machineId;


}
