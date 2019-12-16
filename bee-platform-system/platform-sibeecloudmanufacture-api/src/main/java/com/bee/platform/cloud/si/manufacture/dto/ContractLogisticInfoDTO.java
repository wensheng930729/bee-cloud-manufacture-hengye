package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName ContractLogisticInfoDTO
 * @Description 合同物流记录信息
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同物流记录信息")
public class ContractLogisticInfoDTO {

    @ApiModelProperty("业务类型")
    private Integer businessType;

    @ApiModelProperty("合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("合同编号")
    private String contractNum;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("发货时间")
    private Date departureTime;

    @ApiModelProperty("到货时间")
    private Date arrivalTime;

    @ApiModelProperty("产品名称")
    private String productName;

}
