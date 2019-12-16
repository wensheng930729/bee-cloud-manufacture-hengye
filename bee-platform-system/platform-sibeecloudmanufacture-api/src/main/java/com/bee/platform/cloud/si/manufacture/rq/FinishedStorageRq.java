package com.bee.platform.cloud.si.manufacture.rq;

import java.io.Serializable;

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
@ApiModel(value = "web查询产成品入库请求参数")
public class FinishedStorageRq implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty(value = "是否已入库（0待入库；1已入库）")
	@NotNull(message = "是否已入库不能为空")
	private Integer putStorage;
	
	@ApiModelProperty(value = "吨袋编号")
    private String tonBagNumber;
	
	@ApiModelProperty(value = "炉号")
    private String furnaceNumber;
	
	@ApiModelProperty(value = "班次")
    private String scheduling;
	
    @ApiModelProperty(value = "起始时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String startTime;

    @ApiModelProperty(value = "结束时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private String endTime;

}
