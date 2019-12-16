package com.bee.platform.cloud.si.manufacture.rq;

import com.bee.platform.cloud.si.manufacture.dto.BuyContractSettlePopupCarDTO;
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
@ApiModel(value = "采购合同结算确认弹窗结算参数")
public class BuySettlePopupWindowSaveRq implements Serializable {


	@ApiModelProperty("合同业务id")
	private String contractBusinessId;

	@ApiModelProperty("结算重量")
	private BigDecimal weightSettle;

	@ApiModelProperty("结算单价")
	private BigDecimal unitPriceSettlement;

	@ApiModelProperty("结算信息")
	private List<Long> settles;
}
