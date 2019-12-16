package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @Description 过磅单数据信息查询入参
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/11/12 19:23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "过磅单数据信息查询入参")
public class WeighingListRq implements Serializable {

    @ApiModelProperty(value = "合同编号")
    private String contractNum;

    @ApiModelProperty(value = "车牌号")
    private String trainNumber;

    @ApiModelProperty(value = "称重类型 1采购 2销售")
    private Integer weighingType;

    @ApiModelProperty("开始时间 yyyy-MM-dd")
    private String startTime;

    @ApiModelProperty("结束时间 yyyy-MM-dd")
    private String endTime;

}
