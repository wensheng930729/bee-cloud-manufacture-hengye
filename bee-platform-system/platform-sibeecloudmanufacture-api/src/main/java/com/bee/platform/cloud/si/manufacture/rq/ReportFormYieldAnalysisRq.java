package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * @author qhwang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "产量分析报表请求参数")
public class ReportFormYieldAnalysisRq implements Serializable {

	private static final long serialVersionUID = -2535290764016422422L;

	@ApiModelProperty(value = "生产编号")
	private String productionNo;

	@ApiModelProperty(value = "炉号id")
	private Integer furnaceId;

	@ApiModelProperty(value = "班次id")
	private Integer shiftCode;

	@ApiModelProperty(value = "产品id")
	@NotNull(message = "必须选择产品进行检索!")
	private Integer productId;

	@ApiModelProperty("生产开始时间")
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	private String startTime;

	@ApiModelProperty("生产结束时间")
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	private String endTime;

	@ApiModelProperty(value = "生产日期排序 asc正序 desc倒序")
	private String sort;

}
