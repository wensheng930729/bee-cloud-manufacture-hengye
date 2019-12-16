package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "磅房web端列表请求入参")
public class WeightMachineWebListRq implements Serializable {

    private static final long serialVersionUID = -6000258015103033857L;

    @ApiModelProperty("磅单类型 1 采购 2 销售")
    private Integer type;

    @ApiModelProperty("是否过磅 0 待过磅车辆 1 已过磅车辆")
    @NotNull(message = "未选择待过磅/已过磅")
    @Min(value = 0,message = "待过磅/已过磅未知选项")
    @Max(value = 1,message = "待过磅/已过磅未知选项")
    private Integer isWeight;

    @ApiModelProperty("过磅车辆号")
    private String trainNumber;

    @ApiModelProperty(value = "合同编号")
    private String contractNum;

    @ApiModelProperty("开始时间 yyyy-MM-dd")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String startTime;

    @ApiModelProperty("结束时间 yyyy-MM-dd")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String endTime;

    @ApiModelProperty("供应商（采购）或者客户（销售）id")
    private Integer supOrCustId;

    @ApiModelProperty("日期排序 asc-正序 desc-倒序")
    private String sort;

}
