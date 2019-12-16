package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author jie.zhang
 * @date
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "采购合同新增附件参数")
public class BuyContractAttachmentAddRq implements Serializable {

	private static final long serialVersionUID = -5295923478036237871L;

	@ApiModelProperty("合同名 上传合同文件时传入")
	private String fileName;

	@ApiModelProperty("合同url 上传合同文件时传入")
	private String fileUrl;
}
