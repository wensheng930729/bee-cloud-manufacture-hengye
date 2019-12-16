package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName BuyContractSettleInfoDTO
 * @Description 合同结算详情
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同结算详情")
public class BuyContractSettleInfoDTO {

    @ApiModelProperty("合同信息")
    private BuyContractListContentDTO contractInfoDTO;

    @ApiModelProperty("结算详情")
    private List<BuyContractDetailSettleDTO> settleDTOS;

    @ApiModelProperty("磅房备注")
    private List<PoundHouseDTO> remark;

    @ApiModelProperty("结算状态(0未结算 1已结算)")
    private Integer settleStatus;

}
