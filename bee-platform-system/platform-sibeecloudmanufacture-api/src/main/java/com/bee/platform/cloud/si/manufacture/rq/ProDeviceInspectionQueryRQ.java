package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Date;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "设备巡检查询参数")
public class ProDeviceInspectionQueryRQ implements Serializable {
    private static final long serialVersionUID = -5955127214027578352L;

    @ApiModelProperty("巡检开始时间")
    @NotEmpty(message = "巡检开始时间不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @ApiModelProperty("巡检结束时间")
    @NotEmpty(message = "巡检结束时间不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;
}
