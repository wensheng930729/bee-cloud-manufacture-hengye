package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "巡检设备查询返回信息")
public class DeviceInspectionDTO implements Serializable {
    private static final long serialVersionUID = 487962235755727697L;

    @ApiModelProperty("巡检开始时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @ApiModelProperty("巡检耗时")
    private String costTime;

    @ApiModelProperty("设备巡检id列表")
    private List<Long> ids;

    @ApiModelProperty("本次巡检设备")
    private List<String> totalDevices;

    @ApiModelProperty("检修设备")
    private List<String> repairDevices;

    @ApiModelProperty("异常设备")
    private List<String> abnormalDevices;

}
