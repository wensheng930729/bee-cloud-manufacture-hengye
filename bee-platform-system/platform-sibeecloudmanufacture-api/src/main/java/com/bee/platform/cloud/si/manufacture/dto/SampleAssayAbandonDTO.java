package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liang.li
 * @ClassName SampleAssayAbandonDTO
 * @Description 采购化验已弃用列表DTO
 * @Date 2019/9/23
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "化验已弃用列表DTO")
public class SampleAssayAbandonDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "业务类型")
    private Integer businessType;

    @ApiModelProperty(value = "样品编号")
    private String sampleCode;

    @ApiModelProperty(value = "化验状态")
    private Integer assayStatus;

    @ApiModelProperty(value = "取样时间")
    private Date sampleTime;

    @ApiModelProperty(value = "样品名称")
    private String productName;

    @ApiModelProperty(value = "弃样时间")
    private Date abandonTime;

    @ApiModelProperty(value = "弃样原因")
    private String abandonReason;

    @ApiModelProperty(value = "弃样人id")
    private Integer abandonId;

    @ApiModelProperty(value = "弃样人")
    private String abandonPerson;


}
