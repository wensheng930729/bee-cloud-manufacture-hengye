package com.bee.platform.cloud.si.manufacture.dto;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

@Data
@Accessors(chain = true)
@ApiModel("样品及样品结果详情")
public class SampleResultDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("样品编号")
    private String sampleCode;
    
    @ApiModelProperty("质量要求")
    private String qualityRequirement;
    
    @ApiModelProperty("化验人")
    private String assayPerson;
    
    @ApiModelProperty("取样时间")
    private Date createTime;
    
    @ApiModelProperty("样品名称")
    private String productName;
    
    @ApiModelProperty("规格id")
    private Integer productSpecId;
    
    @ApiModelProperty("规格名称")
    private String productSpecName;
    
    @ApiModelProperty("化验时间")
    private Date assayTime;
    
    @ApiModelProperty("化验指标")
    private List<SampleAssayResultDTO> assays;
    
    @ApiModelProperty("质检主任化验结果--0不合格 1合格")
    private Integer assayResult;
    
    @ApiModelProperty("化验指标")
    private List<CarRemarkDTO> carRemarks;
    
}
