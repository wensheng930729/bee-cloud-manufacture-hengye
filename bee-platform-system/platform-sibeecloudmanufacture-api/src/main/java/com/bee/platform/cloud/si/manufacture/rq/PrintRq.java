package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "调取打印机参数")
public class PrintRq implements Serializable {

	private static final long serialVersionUID = 2746493895204410571L;

	@ApiModelProperty(value = "地磅id")
	private String deviceId;

	@ApiModelProperty(value = "发货单位")
	private String deliveryCompany;

	@ApiModelProperty(value = "司机")
	private String driver;

	@ApiModelProperty(value = "产品Id")
	private Integer productId;

	@ApiModelProperty(value = "产品名称")
	private String productName;

	@ApiModelProperty(value = "车牌号")
	private String trainNumber;

	@ApiModelProperty(value = "进厂重量")
	private BigDecimal inFactoryWeight;

	@ApiModelProperty(value = "出厂重量")
	private BigDecimal outFactoryWeight;

	@ApiModelProperty(value = "净重")
	private BigDecimal netWeight;

	@ApiModelProperty(value = "扣重")
	private BigDecimal deductWeight;

	@ApiModelProperty(value = "业务id")
	private String machineId;

	@ApiModelProperty(value = "1采购2销售")
	private Integer type;
}
