package com.bee.platform.cloud.si.manufacture.rq;

import com.bee.platform.common.entity.Page;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 销售出库搜索请求参数
 * </p>
 *
 * @author MP123
 * @since 2019-10-09
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("销售出库搜索请求参数")
public class SaleOutOfStockSearchRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("公司id")
    private Integer enterpriseId;

    @ApiModelProperty("工厂id")
    private Integer factoryId;

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("客户名称")
    private String customerName;

    @ApiModelProperty("开始时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String startTime;

    @ApiModelProperty("结束时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String endTime;

    @ApiModelProperty("分页对象")
    private Page page;




}
