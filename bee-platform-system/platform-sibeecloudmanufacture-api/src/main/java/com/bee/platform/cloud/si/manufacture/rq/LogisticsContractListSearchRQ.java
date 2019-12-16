package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * 查询物流列表信息搜索请求参数
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "查询物流列表信息搜索请求参数")
public class LogisticsContractListSearchRQ implements Serializable {

    private static final long serialVersionUID = -3902672009198672476L;

    @ApiModelProperty("合同编号")
    private String contractNum;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("合同类型  1采购 2销售")
    private Integer businessType;

    @ApiModelProperty("开始时间 yyyy-MM-dd")
    private String startTime;

    @ApiModelProperty("结束时间 yyyy-MM-dd")
    private String endTime;

    @ApiModelProperty("日期排序 asc-正序 desc-倒序")
    private String sort;

}
