package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("key-value键值队")
public class KeyValueDTO {

	@ApiModelProperty("key值")
    private Integer key;

    @ApiModelProperty("value值")
    private String value;
}
