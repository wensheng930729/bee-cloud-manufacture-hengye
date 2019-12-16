package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅数据信息保存入参
 * @Date 2019/9/23 19:23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "web磅单获取管理的合同信息")
public class WeightMachineWebRelationContractDTO implements Serializable {

    @ApiModelProperty(value = "合同业务id")
    private String contractBusinessId;

    @ApiModelProperty(value = "合同编号")
    private String contractNum;

    @ApiModelProperty(value = "客户/供应商")
    private String custOrSupName;

    @ApiModelProperty(value = "产品名称")
    private String productName;

    @ApiModelProperty(value = "合同数量")
    private BigDecimal quantity;

    @ApiModelProperty(value = "合同金额")
    private BigDecimal amount;

    @ApiModelProperty(value = "合同签订时间")
    private Date signDate;

}
