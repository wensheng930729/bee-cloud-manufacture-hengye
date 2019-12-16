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
 * @author qhwang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "合格率报表请求参数")
public class ReportFormPassRateRq implements Serializable {

	private static final long serialVersionUID = -4551121794018156604L;

	@ApiModelProperty(value = "年份")
	@NotEmpty(message = "年份不能为空!")
	private String year;

	@ApiModelProperty(value = "产品id")
	@NotNull(message = "必须选择产品进行检索!")
	private Integer productId;

}
