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
@ApiModel(value = "质检报表请求参数")
public class ReportFormQualityTestRq implements Serializable {

	private static final long serialVersionUID = 2957927885328212013L;

	@ApiModelProperty(value = "产品类别-仅进出厂有")
	private String categoryId;

	@ApiModelProperty(value = "取样编号")
	private String sampleCode;

	@ApiModelProperty(value = "产品id")
	private Integer productId;

	@ApiModelProperty("开始时间")
	private String startTime;

	@ApiModelProperty("结束时间")
	private String endTime;

	@ApiModelProperty(value = "1生产2进出厂")
	private Integer type;
}
