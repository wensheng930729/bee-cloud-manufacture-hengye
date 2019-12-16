package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName SaleLogisticsInfoDTO
 * @Description 物流信息
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/9/27 9:44
 */
@Data
@Accessors(chain = true)
@ApiModel("物流信息")
public class SaleLogisticsInfoDTO {

    @ApiModelProperty("合同相关信息")
    private SaleContractListContentDTO contractInfoDTO;

    @ApiModelProperty("物流批次信息")
    private List<SaleLogisticsBatchDTO> logisticsBatchDTOS;

}
