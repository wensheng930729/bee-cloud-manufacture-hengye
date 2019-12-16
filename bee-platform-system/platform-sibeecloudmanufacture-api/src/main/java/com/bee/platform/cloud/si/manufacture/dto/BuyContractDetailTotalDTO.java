package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName BuyContractDetailDTO
 * @Description 合同内容详情总计
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同内容详情总计")
public class BuyContractDetailTotalDTO {

    @ApiModelProperty("合同详情")
    private BuyContractDetailDTO contract;

    @ApiModelProperty("付款详情")
    private BuyContractDetailTotalPayDTO pay;

    @ApiModelProperty("结算详情")
    private BuyContractDetailTotalSettleDTO settle;

    @ApiModelProperty("批次详情")
    private List<BuyContractBatchDTO> batch;

    @ApiModelProperty("磅房备注")
    private List<PoundHouseDTO> remark;
}
