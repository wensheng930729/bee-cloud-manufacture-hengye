package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author qhwang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "确认合同批次参数")
public class SureLogisticsRq implements Serializable {

	private static final long serialVersionUID = -8296096455964000874L;

	@ApiModelProperty("批次id")
	private String batchId;

}
