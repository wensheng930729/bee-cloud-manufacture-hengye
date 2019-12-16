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
 * @ClassName BuyContractDetailDTO
 * @Description 合同内容详情
 * @author
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("合同内容详情")
public class BuyContractDetailDTO {

    @ApiModelProperty("合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("合同编号")
    private String contractNum;

    @ApiModelProperty("签订日期")
    @JsonFormat(pattern = "yyyy年MM月dd日")
    private Date signDate;

    @ApiModelProperty("供应商")
    private String supplierName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("起始地")
    private String originAddress;

    @ApiModelProperty("合同金额")
    private BigDecimal amount;

    @ApiModelProperty("合同单价")
    private BigDecimal unitPrice;

    @ApiModelProperty("联系人")
    private String linkMan;

    @ApiModelProperty("联系人号码")
    private String linkPhone;

    @ApiModelProperty("确认方  0我方确认 1供应商确认")
    private Integer confirmPart;

    @ApiModelProperty("采购方式  0自提 1供方发货")
    private Integer purchaserMode;

    @ApiModelProperty("质量要求")
    private String qualityRequirement;

    @ApiModelProperty("合同数量")
    private BigDecimal quantity;

    @ApiModelProperty("在途量")
    private BigDecimal trafficVolume;

    @ApiModelProperty("已到")
    private BigDecimal arrivalVolume;

    @ApiModelProperty("已发")
    private BigDecimal issuedVolume;

    @ApiModelProperty("未发")
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
