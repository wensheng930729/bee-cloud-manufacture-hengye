package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName SaleContractDetailTotalDTO
 * @Description 销售合同内容详情总计
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售合同内容详情总计")
public class SaleContractDetailTotalDTO {

    @ApiModelProperty("合同详情")
    private SaleContractDetailDTO contract;

    @ApiModelProperty("收款详情")
    private SaleContractDetailTotalReceiveDTO receive;

    @ApiModelProperty("结算详情")
    private BuyContractDetailTotalSettleDTO settle;

    @ApiModelProperty("批次详情")
    private List<SaleContractBatchDTO> batch;
}
