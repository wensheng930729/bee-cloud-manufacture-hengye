package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: ConfirmProductDTO
 * @Description: 磅单号
 * @Author: fei.sun
 * @Date: 2019/9/29 17:41
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("磅单号")
public class ConfirmProductDTO {

    @ApiModelProperty("磅单号")
    private String machineId;

}

