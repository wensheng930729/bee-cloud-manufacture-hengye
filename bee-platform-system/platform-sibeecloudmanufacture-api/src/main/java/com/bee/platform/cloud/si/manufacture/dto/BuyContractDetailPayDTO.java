package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName BuyContractDetailPayDTO
 * @Description 合同内容付款详情情况
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同内容付款详情情况")
public class BuyContractDetailPayDTO {

    @ApiModelProperty("付款序号名")
    private String serialNum;

    @ApiModelProperty("付款时间")
    private Date payTime;

    @ApiModelProperty("付款金额")
    private BigDecimal payAmount;

}
