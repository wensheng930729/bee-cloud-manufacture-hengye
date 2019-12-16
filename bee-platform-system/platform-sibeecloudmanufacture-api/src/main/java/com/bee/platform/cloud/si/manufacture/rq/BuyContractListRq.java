package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "合同列表请求参数")
public class BuyContractListRq implements Serializable {

	private static final long serialVersionUID = -309991063429778478L;

	@ApiModelProperty("合同编号")
	private String contractNum;
	
	@ApiModelProperty("产品id")
	private Integer productId;

	@ApiModelProperty("开始时间 yyyy-MM-dd")
	private String startTime;

	@ApiModelProperty("结束时间 yyyy-MM-dd")
	private String endTime;

	@ApiModelProperty("是否完成 0否 1是")
	@NotNull(message="是否完成不能为空")
	private Integer completed;

}
