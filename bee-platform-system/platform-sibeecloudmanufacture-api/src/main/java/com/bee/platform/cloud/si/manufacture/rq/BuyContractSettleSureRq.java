package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author qhwang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "采购合同结算确认参数")
public class BuyContractSettleSureRq implements Serializable {

	private static final long serialVersionUID = 989632449782869806L;

	@ApiModelProperty("结算业务id")
	private String contractSettlementBusinessId;

	@ApiModelProperty("合同业务id")
	private String contractBusinessId;

}
