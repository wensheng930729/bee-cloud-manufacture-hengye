package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

/**
 * @author xin.huang
 * @description 生产数据统计请求参数
 * @date 2019/10/18
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "生产数据统计请求参数")
public class ProStatisticRQ implements Serializable {
    private static final long serialVersionUID = -96547571869499439L;

    @ApiModelProperty(value = "时间范围:1本日，2本周，3本月，4本年")
    private Integer timeRange;

    @ApiModelProperty(value = "开始时间")
    private String startTime;

    @ApiModelProperty(value = "结束时间")
    private String endTime;

    @ApiModelProperty(value = "产品id")
    private Integer productId;

    @ApiModelProperty(value = "炉号id")
    private Integer furnaceId;

    @ApiModelProperty(value = "公司id")
    private Integer enterpriseId;

    @ApiModelProperty(value = "工厂id")
    private Integer factoryId;

    @ApiModelProperty(value = "料批类型：0主料，1辅料")
    private Integer materialType;

    @ApiModelProperty(value = "当前时间")
    private String currentTime;
}
