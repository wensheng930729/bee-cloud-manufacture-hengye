package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName BuyNewLogisticsDTO
 * @Description 物流批次相关信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/9/27 9:44
 */
@Data
@Accessors(chain = true)
@ApiModel("物流批次相关信息")
public class BuyNewLogisticsDTO {

    @ApiModelProperty("合同相关信息")
    private BuyContractListContentDTO contractInfoDTO;

    @ApiModelProperty("物流批次信息")
    private List<BuyLogisticsBatchDTO> logisticsBatchDTOS;

}
