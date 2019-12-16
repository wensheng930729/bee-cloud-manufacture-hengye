package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "采购合同修改结算金额参数")
public class BuyContractSettleUpdateAmountRq implements Serializable {


	private static final long serialVersionUID = -2943047804669971272L;

	@ApiModelProperty("结算业务id")
	private String contractSettlementBusinessId;

	@ApiModelProperty("结算单价")
	private BigDecimal unitPriceSettlement;

	@ApiModelProperty("结算金额")
	private BigDecimal amountSettlement;

}
