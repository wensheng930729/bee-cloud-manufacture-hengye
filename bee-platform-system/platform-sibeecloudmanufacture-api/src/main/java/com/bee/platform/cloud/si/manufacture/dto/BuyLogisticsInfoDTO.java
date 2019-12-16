package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @ClassName BuyLogisticsBatchDTO
 * @Description 物流信息
 * @author qh.wang
 * @version 1.0.0
 * @Date 2019/9/25 9:44
 */
@Data
@Accessors(chain = true)
@ApiModel("物流信息")
public class BuyLogisticsInfoDTO {

    @ApiModelProperty("合同相关信息")
    private BuyContractListContentDTO contractInfoDTO;

    @ApiModelProperty("物流批次信息")
    private List<BuyLogisticsBatchDTO> logisticsBatchDTOS;

}
