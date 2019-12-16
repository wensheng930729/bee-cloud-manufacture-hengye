package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @ClassName BuyContractSettleListDTO
 * @Description 采购结算列表
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("采购结算列表")
public class BuyContractSettleListDTO {

    @ApiModelProperty("已结算id")
    private Long settleId;

    @ApiModelProperty("合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("合同编号")
    private String contractNum;

    @ApiModelProperty("供应商")
    private String supplierName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("车辆数")
    private Integer carNum;

    @ApiModelProperty("签订日期")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date signDate;

    @ApiModelProperty("结算日期")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date settleTime;

}
