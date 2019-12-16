package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @ClassName BuyContractDetailTotalSettleDTO
 * @Description 合同内容结算情况
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同内容付款情况")
public class BuyContractDetailTotalPayDTO {

    @ApiModelProperty("合计付款情况")
    private BigDecimal total;

    @ApiModelProperty("结算数量")
    private List<BuyContractDetailPayDTO> data;

}
