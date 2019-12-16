package com.bee.platform.cloud.si.manufacture.rq;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonFormat;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "web查询采购待入库请求参数")
public class BuyStorageRq implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty(value = "合同编号")
    private String contractNum;

	@ApiModelProperty(value = "供应商名称")
    private String supplierName;

    @ApiModelProperty(value = "起始时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String startTime;

    @ApiModelProperty(value = "结束时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String endTime;
}
