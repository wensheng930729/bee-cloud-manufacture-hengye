package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "采购结算列表请求参数")
public class BuySettleListRq implements Serializable {

	private static final long serialVersionUID = 227881721326769413L;

	@ApiModelProperty("合同编号")
	private String contractNum;

	@ApiModelProperty("供应商")
	private String supplierName;

	@ApiModelProperty("产品名称")
	private String productName;

	@ApiModelProperty("签订日期开始")
	private String signDateStart;

	@ApiModelProperty("签订日期结束")
	private String signDateEnd;

	@ApiModelProperty("结算日期开始")
	private String settleTimeStart;

	@ApiModelProperty("结算日期结束")
	private String settleTimeEnd;

	@ApiModelProperty("0未结算 1已结算")
	@NotNull(message="结算状态不能为空")
	private Integer settleStatus;
}
