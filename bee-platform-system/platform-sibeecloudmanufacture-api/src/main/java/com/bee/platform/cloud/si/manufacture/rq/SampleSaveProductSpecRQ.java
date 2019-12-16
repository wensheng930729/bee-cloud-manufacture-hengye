package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

@Data
@Accessors(chain = true)
@ApiModel("样品化验保存规格RQ")
public class SampleSaveProductSpecRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("样品编号")
    @NotBlank(message = "样品编号不能为空")
    private String sampleCode;

    @ApiModelProperty(value = "产品规格id")
    @NotNull(message = "产品规格id不能为空")
    private Integer productSpecId;

    @ApiModelProperty(value = "产品规格名称")
    @NotBlank(message = "产品规格不能为空")
    private String productSpecName;

}
