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
@ApiModel(value = "新增合同批次参数")
public class NewLogisticsRq implements Serializable {

	private static final long serialVersionUID = -146637400535485217L;

	@ApiModelProperty("合同业务id")
	private String contractBusinessId;

}
