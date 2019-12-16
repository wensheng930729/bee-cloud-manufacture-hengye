package com.bee.platform.cloud.si.manufacture.dto;

import com.bee.platform.cloud.si.manufacture.rq.BuyContractAttachmentAddRq;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @ClassName SaleContractDetailDTO
 * @Description 合同内容详情
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("销售合同内容详情")
public class SaleContractDetailDTO {

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

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("到达地")
    private String arrivalAddress;

    @ApiModelProperty("合同金额")
    private BigDecimal amount;

    @ApiModelProperty("合同单价")
    private BigDecimal unitPrice;

    @ApiModelProperty("联系人")
    private String linkMan;

    @ApiModelProperty("联系人号码")
    private String linkPhone;

    @ApiModelProperty("确认方  0我方确认 1客户确认")
    private Integer confirmPart;

    @ApiModelProperty("采购方式  0自提 1包运")
    private Integer saleMode;

    @ApiModelProperty("质量要求")
    private String qualityRequirement;

    @ApiModelProperty("合同数量")
    private BigDecimal quantity;

    @ApiModelProperty("在途量")
    private BigDecimal trafficVolume;

    @ApiModelProperty("已收货量")
    private BigDecimal receivedVolume;

    @ApiModelProperty("已发货量")
    private BigDecimal issuedVolume;

    @ApiModelProperty("未发量")
    private BigDecimal undeliveredVolume;

    @ApiModelProperty("合同名附件")
    private List<BuyContractAttachmentAddRq> files;

    @ApiModelProperty("完成状态0否 1是")
    private Integer completed;

    @ApiModelProperty("创建人")
    private String creator;

    @ApiModelProperty("合同类型  0短期合同 1长期协议")
    private Integer type;
}
