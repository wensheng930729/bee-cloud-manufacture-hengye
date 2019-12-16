package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liang.li
 * @ClassName SampleAssayDetailDTO
 * @Description 取样化验详情DTO
 * @Date 2019/9/27
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "取样化验详情DTO")
public class SampleAssayDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "业务类型")
    private Integer businessType;
    @ApiModelProperty(value = "样品编号")
    private String sampleCode;
    @ApiModelProperty(value = "样品id")
    private Integer productId;
    @ApiModelProperty(value = "样品名称")
    private String productName;
    @ApiModelProperty(value = "取样时间")
    private Date sampleTime;
    @ApiModelProperty(value = "取样人")
    private String samplePerson;

}
