package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;

@Data
@Accessors(chain = true)
@ApiModel(value = "扫描二维码查询-采购")
public class DetailBySampleCodeBuyDTO {

    @ApiModelProperty(value = "1采购2销售3生产4产品装袋")
    private Integer sampleType;

    @ApiModelProperty(value = "样品编码")
    private String sampleCode;

    @ApiModelProperty(value = "样品ID")
    private Integer productId;

    @ApiModelProperty(value = "样品名称")
    private String productName;

    @ApiModelProperty(value = "样品规格id")
    private Integer productSpecId;

    @ApiModelProperty(value = "样品规格名称")
    private String productSpecName;

    @ApiModelProperty(value = "化验人id")
    private Integer assayId;

    @ApiModelProperty(value = "化验人")
    private String assayPerson;

    @ApiModelProperty(value = "化验时间")
    @JsonFormat(pattern = "yyyy年MM月dd日 HH:mm:ss")
    private Date assayTime;

    @ApiModelProperty(value = "化验结果")
    private List<SampleAssayResultOutDTO> assayResultList;

    @ApiModelProperty(value = "合同仓库详情")
    private List<DetailContractStorageBuyDTO> contractStorageDetailList;

}
