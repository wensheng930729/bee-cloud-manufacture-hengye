package com.bee.platform.cloud.si.manufacture.dto;

import java.util.List;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
@ApiModel("同磅单下的样品信息")
public class BuyCarSampleMsgDTO {

    @ApiModelProperty("合同编号")
    private String contractNum;

    @ApiModelProperty("合同业务id")
    private String contractBusinessId;
    
    @ApiModelProperty("产品id")
    private Integer productId;
    
    @ApiModelProperty("产品名称")
    private String productName;
    
    @ApiModelProperty("规格id")
    private Integer productSpecId;
    
    @ApiModelProperty("规格名称")
    private String productSpecName;
    
    @ApiModelProperty("质量要求")
    private String qualityRequirement;
    
	@ApiModelProperty("磅单备注")
    private String remark;
    
    @ApiModelProperty("样品列表")
    private List<SampleResultDTO> sampleResultDTOs;
    
}
