package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

@Data
@Accessors(chain = true)
@ApiModel("仓库树")
public class StorageTreeDTO {

    @ApiModelProperty("仓库类型Id")
    private Integer StorageTypeId;

	@ApiModelProperty("仓库类型名称")
    private String StorageTypeName;

    @ApiModelProperty("不同仓库类型下的具体仓库值")
    private List<KeyValueDTO> Storages;

}
