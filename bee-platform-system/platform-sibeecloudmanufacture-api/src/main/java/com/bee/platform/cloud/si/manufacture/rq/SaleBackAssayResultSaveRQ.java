package com.bee.platform.cloud.si.manufacture.rq;

import java.io.Serializable;
import java.util.List;

import com.bee.platform.cloud.si.manufacture.dto.SampleAssayResultDTO;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("反馈化验结果保存RQ")
public class SaleBackAssayResultSaveRQ implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@ApiModelProperty("承运方运输车次ID")
    private List<String> carrierTransportDetailIds;

    @ApiModelProperty("化验结果")
    private List<SampleAssayResultDTO> resultList;
    
    @ApiModelProperty("0-不合格；1-合格")
    private Integer assayResult;

}
