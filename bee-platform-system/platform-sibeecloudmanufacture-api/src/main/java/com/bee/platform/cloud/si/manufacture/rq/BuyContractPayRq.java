package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "采购合同付款参数")
public class BuyContractPayRq implements Serializable {

	private static final long serialVersionUID = 1198670372593526845L;

	@ApiModelProperty("合同业务id")
	private String contractBusinessId;

	@ApiModelProperty("付款金额")
	private BigDecimal payAmount;

	@ApiModelProperty("付款日期")
	private String payTime;

}
