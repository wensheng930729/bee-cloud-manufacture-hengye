package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/10/14
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "设备巡检结果确认")
public class ProDeviceInspectionConfirmRQ implements Serializable {
    private static final long serialVersionUID = 6726790106806443489L;

    @ApiModelProperty("设备巡检id列表")
    private List<Long> ids;
}
