package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author xin.huang
 * @description
 * @date 2019/10/22
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "设备巡检统计返回信息")
public class ProDeviceInspectionStatisticDTO implements Serializable {
    private static final long serialVersionUID = 157783193502921909L;

    @ApiModelProperty("今日巡检")
    private Integer currentCount;

    @ApiModelProperty("今日待检")
    private Integer waitCheck;

    @ApiModelProperty("检修设备")
    private Integer checked;

    @ApiModelProperty("正常运行")
    private Integer normal;

    @ApiModelProperty("异常待修")
    private Integer abnormal;

    @ApiModelProperty("今日巡检率")
    private BigDecimal checkRate;

    @ApiModelProperty("设备运行率")
    private BigDecimal runRate;

    @ApiModelProperty("总设备数")
    private Integer total;


}
