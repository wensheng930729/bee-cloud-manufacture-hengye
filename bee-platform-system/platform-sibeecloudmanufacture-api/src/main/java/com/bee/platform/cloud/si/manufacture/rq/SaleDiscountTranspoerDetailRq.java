package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author qhwang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "销售被折价车辆信息")
public class SaleDiscountTranspoerDetailRq implements Serializable {

	private static final long serialVersionUID = -113896379395727953L;

	@ApiModelProperty("被折价的承运方运输车次ID集合")
	private List<String> carrierTransportDetailIds;

	@ApiModelProperty("折扣单价")
	@NotNull(message = "折扣单价不能为空")
	private BigDecimal discountUnitPrice;

}
