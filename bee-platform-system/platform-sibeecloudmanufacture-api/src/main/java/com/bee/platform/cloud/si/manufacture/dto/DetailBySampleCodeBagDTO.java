package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

@Data
@Accessors(chain = true)
@ApiModel(value = "扫描二维码查询-吨袋")
public class DetailBySampleCodeBagDTO {

    @ApiModelProperty(value = "1采购2销售3生产4产品装袋")
    private Integer sampleType;

    @ApiModelProperty(value = "吨袋编码")
    private String baggingCode;

    @ApiModelProperty(value = "成品ID")
    private Integer productId;

    @ApiModelProperty(value = "成品名称")
    private String productName;

    @ApiModelProperty(value = "成品规格id")
    private Integer productSpecId;

    @ApiModelProperty(value = "成品规格名称")
    private String productSpecName;

    @ApiModelProperty(value = "重量")
    private BigDecimal amount;

    @ApiModelProperty(value = "仓库id")
    private Integer storageId;

    @ApiModelProperty(value = "仓库名称")
    private String storageName;

    @ApiModelProperty(value = "炉号id")
    private Integer furnaceId;

    @ApiModelProperty(value = "炉号名称")
    private String furnaceName;

    @ApiModelProperty(value = "班次编码")
    private Integer shiftCode;

    @ApiModelProperty(value = "班次")
    private String shift;

    @ApiModelProperty(value = "炉次编码")
    private Integer furnaceBatchCode;

    @ApiModelProperty(value = "炉次")
    private String furnaceBatch;

    @ApiModelProperty(value = "装袋时间")
    @JsonFormat(pattern = "yyyy年MM月dd日 HH:mm:ss")
    private Date bagTime;

    @ApiModelProperty(value = "样品编码")
    private String sampleCode;

    @ApiModelProperty(value = "样品ID")
    private Integer sampleId;

    @ApiModelProperty(value = "样品名称")
    private String sampleName;

    @ApiModelProperty(value = "样品规格id")
    private Integer sampleSpecId;

    @ApiModelProperty(value = "样品规格名称")
    private String sampleSpecName;

    @ApiModelProperty(value = "化验人id")
    private Integer assayId;

    @ApiModelProperty(value = "化验人")
    private String assayPerson;

    @ApiModelProperty(value = "化验时间")
    @JsonFormat(pattern = "yyyy年MM月dd日 HH:mm:ss")
    private Date assayTime;

    @ApiModelProperty(value = "化验结果")
    private List<SampleAssayResultOutDTO> assayResultList;

}
