package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName SampleAssayStartRQ
 * @Description 采购样品开始化验请求rq
 * @Date 2019/9/23
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "采购样品开始化验请求rq")
public class SampleAssayStartRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "样品编号")
    @NotBlank
    private String sampleCode;

}
