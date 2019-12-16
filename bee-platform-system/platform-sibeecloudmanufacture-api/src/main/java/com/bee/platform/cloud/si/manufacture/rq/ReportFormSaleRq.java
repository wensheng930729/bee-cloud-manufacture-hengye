package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "销售报表请求参数")
public class ReportFormSaleRq implements Serializable {

	private static final long serialVersionUID = -7544667861275973681L;

	@ApiModelProperty(value = "采购商")
	private String customerName;

	@ApiModelProperty(value = "产品")
	private Integer productId;

	@ApiModelProperty(value = "合同编号")
	private String contractNum;

	@ApiModelProperty("开始时间")
	private String startTime;

	@ApiModelProperty("结束时间")
	private String endTime;

}
