package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "原料日报表请求参数")
public class ReportRawMaterialRq implements Serializable {

	private static final long serialVersionUID = 1390417723115888866L;

	@ApiModelProperty(value = "产品（此处传入的是产品Id）")
	@NotNull(message = "productId不能为空")
	private Integer productId;

	@ApiModelProperty("日报日期")
	@DateTimeFormat(pattern = "yyyy-MM-dd")
	@JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
	private Date dayTime;

}
