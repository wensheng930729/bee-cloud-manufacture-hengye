package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author qh.wang
 * @version 1.0.0
 * @ClassName CarrierInfoDTO
 * @Description 承运商信息
 * @Date 2019/10/9 15:46
 */
@Data
@Accessors(chain = true)
@ApiModel("承运商信息")
public class CarrierInfoDTO {

    @ApiModelProperty("承运方id")
    private Long carrierId;

    @ApiModelProperty("承运方")
    private String carrierName;

}
