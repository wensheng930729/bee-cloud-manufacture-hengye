package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author liang.li
 * @ClassName SampleAssayAlreadyDTO
 * @Description 采购已化验样品列表DTO
 * @Date 2019/9/25
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "已化验样品列表DTO")
public class SampleAssayAlreadyDTO implements Serializable {

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

    @ApiModelProperty(value = "样品规格id")
    private Integer productSpecId;

    @ApiModelProperty(value = "样品规格名称")
    private String productSpecName;

    @ApiModelProperty(value = "化验时间")
    private Date assayTime;

    @ApiModelProperty(value = "化验结果")
    private List<SampleAssayResultDTO> assayResultList;

}
