package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "产量统计报表请求参数")
public class ReportFormOutputStatisticsRq implements Serializable {

	private static final long serialVersionUID = -4834297150619031208L;

	@ApiModelProperty(value = "检索类型 1-产量统计 2-消耗分析")
	@NotNull(message = "缺少检索类型！")
	private Integer type;

	@ApiModelProperty(value = "产品id")
	@NotNull(message = "必须选择产品进行检索!")
	private Integer productId;

	@ApiModelProperty(value = "炉号id 全部：0")
	@NotNull(message = "必须选择炉号进行检索!")
	private Integer furnaceId;

	@ApiModelProperty("开始时间")
	private String startTime;

	@ApiModelProperty("结束时间")
	private String endTime;

}
