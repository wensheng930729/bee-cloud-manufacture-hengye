package com.bee.platform.cloud.si.manufacture.rq;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "查看不合格车辆信息请求参数")
public class CarListRQ implements Serializable {
	private static final long serialVersionUID = 1L;
	
    @ApiModelProperty("批次id")
    @NotNull(message="批次id不能为空")
    private String batchId;
    
    @ApiModelProperty("合同id")
    @NotNull(message="合同id不能为空")
    private String contractBusinessId;

}
