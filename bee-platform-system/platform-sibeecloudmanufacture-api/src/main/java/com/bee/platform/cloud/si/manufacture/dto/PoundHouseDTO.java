package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(value = "磅房备注dto")
public class PoundHouseDTO {

	@ApiModelProperty(value = "车牌号")
    private String trainNumber;

	@ApiModelProperty(value = "备注")
    private String remark;

}
