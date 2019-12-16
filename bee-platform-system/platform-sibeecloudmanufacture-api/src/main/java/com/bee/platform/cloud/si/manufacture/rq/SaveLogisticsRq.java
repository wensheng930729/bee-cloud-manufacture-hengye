package com.bee.platform.cloud.si.manufacture.rq;

import com.bee.platform.cloud.si.manufacture.dto.BuyTransportSectionDTO;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author qhwang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "保存合同批次参数")
public class SaveLogisticsRq implements Serializable {

	private static final long serialVersionUID = 6827063043122276123L;

	@ApiModelProperty("合同业务id")
	private String contractBusinessId;

	@ApiModelProperty("批次id")
	private String batchId;

	@ApiModelProperty("物流批次运输段信息")
	private List<SaveTransportSectionRq> transportSectionDTOS;

}
