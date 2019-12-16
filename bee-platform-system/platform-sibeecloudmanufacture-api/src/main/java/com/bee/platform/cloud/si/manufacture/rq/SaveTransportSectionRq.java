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
@ApiModel(value = "新增批次阶段参数")
public class SaveTransportSectionRq implements Serializable {

	private static final long serialVersionUID = -7444780616649091698L;

	@ApiModelProperty("运输段")
	private Integer transportSection;

	@ApiModelProperty("第几段运输")
	private String transportSectionName;

	@ApiModelProperty("运输方式(1-汽车 2-轮船 3-火车)")
	private Integer transportMode;

	@ApiModelProperty("是否到厂(0-不到厂 1-到厂)")
	private Integer toFactory;

	@ApiModelProperty("起始地地点id")
	private Integer startingPlaceId;

	@ApiModelProperty("到达地地点id")
	private Integer arrivalPlaceId;

}
