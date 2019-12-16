package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liang.li
 * @ClassName SampleAssayPrepareAndAssayingDTO
 * @Description 采购待化验和化验中列表DTO
 * @Date 2019/9/23
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "待化验和化验中列表DTO")
public class SampleAssayPrepareAndAssayingDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "业务类型")
    private Integer businessType;

    @ApiModelProperty(value = "样品编号")
    private String sampleCode;

    @ApiModelProperty(value = "化验状态")
    private Integer assayStatus;

    @ApiModelProperty(value = "取样时间")
    private Date sampleTime;

    @ApiModelProperty(value = "样品id")
    private Integer productId;

    @ApiModelProperty(value = "样品名称")
    private String productName;

}
