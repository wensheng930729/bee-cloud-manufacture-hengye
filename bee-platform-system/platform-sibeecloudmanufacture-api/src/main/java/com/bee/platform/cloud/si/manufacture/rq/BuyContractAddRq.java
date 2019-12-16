package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "采购合同新增参数")
public class BuyContractAddRq implements Serializable {

	private static final long serialVersionUID = -9086752127004201280L;

	@ApiModelProperty("合同编号")
	private String contractNum;
	
	@ApiModelProperty("供应商id")
	private Integer supplierId;
	
	@ApiModelProperty("签订日期")
	private String signDate;

	@ApiModelProperty("产品id")
	private Integer productId;

	@ApiModelProperty("产品名称")
	private String productName;

	@ApiModelProperty("地点Id")
	private Integer addressId;

	@ApiModelProperty("数量")
	private BigDecimal quantity;

	@ApiModelProperty("金额")
	private BigDecimal amount;

	@ApiModelProperty("单价")
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

	@ApiModelProperty("合同类型  0短期合同 1长期协议")
	private Integer type;

	@ApiModelProperty("合同名 上传合同文件时传入")
	private List<BuyContractAttachmentAddRq> files;
}
