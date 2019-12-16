package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;

/**
 * @ClassName SaleContractListContentDTO
 * @Description 合同列表内容
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售合同列表内容")
public class SaleContractListContentDTO {

    @ApiModelProperty("合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("合同编号")
    private String contractNum;

    @ApiModelProperty("客户")
    private String customerName;

    @ApiModelProperty("签订日期")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date signDate;

    @ApiModelProperty("交货日期")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date deliveryDate;

    @ApiModelProperty("录入日期")
    private Date createTime;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("到达地")
    private String arrivalAddress;

    @ApiModelProperty("确认方  0我方确认 1客户确认")
    private Integer confirmPart;

    @ApiModelProperty("销售方式  0自提 1包运")
    private Integer saleMode;

    @ApiModelProperty("质量要求")
    private String qualityRequirement;

    @ApiModelProperty("合同数量")
    private BigDecimal quantity;

    @ApiModelProperty("合同金额")
    private BigDecimal amount;

    @ApiModelProperty("在途量")
    private BigDecimal trafficVolume;

    @ApiModelProperty("已收货量")
    private BigDecimal receivedVolume;

    @ApiModelProperty("已发量")
    private BigDecimal issuedVolume;

    @ApiModelProperty("未发量")
    private BigDecimal undeliveredVolume;

    @ApiModelProperty("完成数量")
    private BigDecimal completedVolume;

    @ApiModelProperty("完成状态0进行中 1已完成")
    private Integer completed;

    @ApiModelProperty("批次数量")
    private Integer logisticsCount;

}
