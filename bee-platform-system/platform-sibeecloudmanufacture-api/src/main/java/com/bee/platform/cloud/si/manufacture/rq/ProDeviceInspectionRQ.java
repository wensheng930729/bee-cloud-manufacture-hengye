package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "巡检设备新增参数")
public class ProDeviceInspectionRQ implements Serializable {
    private static final long serialVersionUID = -4231987881831854329L;

    @ApiModelProperty("设备编号")
    private String code;

    @ApiModelProperty("设备名称")
    private String name;

    @ApiModelProperty("巡检项目")
    private String inspectionItem;

    @ApiModelProperty("设备状态：1正常，2检修，3异常")
    private Integer state;

    @ApiModelProperty("已检修时间")
    private String checkTime;

    @ApiModelProperty("检修概况")
    private String remark;
}
