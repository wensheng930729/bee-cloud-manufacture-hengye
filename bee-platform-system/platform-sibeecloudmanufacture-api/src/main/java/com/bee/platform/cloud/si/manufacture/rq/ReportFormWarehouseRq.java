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
@ApiModel(value = "产成品入库报表请求参数")
public class ReportFormWarehouseRq implements Serializable {

	private static final long serialVersionUID = 6641128305267152626L;

	@ApiModelProperty(value = "产品id")
	private Integer productId;

	@ApiModelProperty(value = "仓库id")
	private Integer storageId;

	@ApiModelProperty("开始时间")
	private String startTime;

	@ApiModelProperty("结束时间")
	private String endTime;

}
