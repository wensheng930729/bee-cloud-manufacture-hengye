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
@ApiModel(value = "销售合同收款参数")
public class SaleContractReceiveRq implements Serializable {


	private static final long serialVersionUID = 2363454656655859335L;

	@ApiModelProperty("合同业务id")
	private String contractBusinessId;

	@ApiModelProperty("收款金额")
	private BigDecimal paymentAmount;

	@ApiModelProperty("收款日期")
	private String receiveTime;

}
