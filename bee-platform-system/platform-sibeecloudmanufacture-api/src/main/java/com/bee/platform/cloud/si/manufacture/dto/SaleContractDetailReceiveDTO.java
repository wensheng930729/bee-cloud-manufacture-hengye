package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName SaleContractDetailReceiveDTO
 * @Description 销售合同内容收款详情情况
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售合同内容收款详情情况")
public class SaleContractDetailReceiveDTO {

    @ApiModelProperty("收款序号名")
    private String serialNum;

    @ApiModelProperty("收款时间")
    private Date receiveTime;

    @ApiModelProperty("结款金额")
    private BigDecimal paymentAmount;

}
