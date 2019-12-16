package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "查询销售运输台账信息搜索请求参数")
public class SaleCarrierTransportSearchRQ implements Serializable {

    private static final long serialVersionUID = -12332114653690633L;

    @ApiModelProperty("客户名")
    private String customerName;

    @ApiModelProperty("产品")
    private String productName;

    @ApiModelProperty("开始时间 yyyy-MM-dd")
    private String startTime;

    @ApiModelProperty("结束时间 yyyy-MM-dd")
    private String endTime;

    @ApiModelProperty("日期排序 asc-正序 desc-倒序")
    private String sort;

}
