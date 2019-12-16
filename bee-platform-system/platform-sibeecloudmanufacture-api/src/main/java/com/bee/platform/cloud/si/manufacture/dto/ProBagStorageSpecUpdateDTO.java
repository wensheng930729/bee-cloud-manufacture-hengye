package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel(value = "生产装袋入库规格更新DTO")
public class ProBagStorageSpecUpdateDTO {

    @ApiModelProperty(value = "吨袋编码")
    private String baggingCode;

    @ApiModelProperty(value = "产品规格id")
    private Integer productSpecId;

    @ApiModelProperty(value = "产品规格名称")
    private String productSpecName;

}
