package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author liang.li
 * @ClassName SampleSaleAddContractBusinessIdDTO
 * @Description 销售取样需要添加合同业务id的DTO
 * @Date 2019/10/10
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "销售取样需要添加合同业务id的DTO")
public class SampleSaleAddContractBusinessIdDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "样品id")
    private Integer id;

    @ApiModelProperty(value = "样品编号")
    private String sampleCode;

    @ApiModelProperty(value = "合同业务id")
    private String contractBusinessId;

    @ApiModelProperty(value = "吨袋编号")
    private String tonCode;



}
