package com.bee.platform.cloud.si.manufacture.rq;

import java.io.Serializable;
import java.util.Date;

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
@ApiModel(value = "采购付款情况rq")
public class LookBoardRQ implements Serializable {

	private static final long serialVersionUID = 1L;

	@ApiModelProperty(value = "类型1主料2辅料3成品4供应商")
    @NotNull
    private Integer type;

    @ApiModelProperty(value = "开始时间")
    @NotNull
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date startTime;

    @ApiModelProperty(value = "结束时间")
    @NotNull
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date endTime;
}
