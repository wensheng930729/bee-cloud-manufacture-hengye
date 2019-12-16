package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName BuyContractListContentDTO
 * @Description 合同列表内容
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同列表内容")
public class BuyContractListContentDTO {

    @ApiModelProperty("合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("合同编号")
    private String contractNum;

    @ApiModelProperty("签订日期")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date signDate;

    @ApiModelProperty("录入日期")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date createTime;

    @ApiModelProperty("供应商")
    private String supplierName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("确认方  0我方确认 1供应商确认")
    private Integer confirmPart;

    @ApiModelProperty("采购方式  0自提 1供方发货")
    private Integer purchaserMode;

    @ApiModelProperty("质量要求")
    private String qualityRequirement;

    @ApiModelProperty("合同数量")
    private BigDecimal quantity;

    @ApiModelProperty("合同金额")
    private BigDecimal amount;

    @ApiModelProperty("在途量")
    private BigDecimal trafficVolume;

    @ApiModelProperty("到货量")
    private BigDecimal arrivalVolume;

    @ApiModelProperty("已发量")
    private BigDecimal issuedVolume;

    @ApiModelProperty("未发量")
    private BigDecimal undeliveredVolume;

    @ApiModelProperty("完成数量")
    private BigDecimal completedVolume;

    @ApiModelProperty("总结算数量")
    private BigDecimal settlementVolume;

    @ApiModelProperty("批次数量")
    private Integer logisticsCount;

    @ApiModelProperty("联系人")
    private String linkMan;

    @ApiModelProperty("产品单位")
    private String unitValue;

    @ApiModelProperty("完成状态0进行中 1已完成")
    private Integer completed;

}
