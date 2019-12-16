package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName SampleAssayAbandonRQ
 * @Description 采购样品弃用请求rq
 * @Date 2019/9/23
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "采购样品弃用请求rq")
public class SampleAssayAbandonRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "业务类型1采购2销售3生产")
    @NotNull
    private Integer businessType;

    @ApiModelProperty(value = "样品编号")
    @NotBlank(message = "样品编号不能为空")
    private String sampleCode;

    @ApiModelProperty(value = "弃用原因")
    @NotBlank(message = "弃用原因不能为空")
    private String abandonReason;

}
