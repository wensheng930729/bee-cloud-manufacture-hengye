package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.annotations.ApiOperation;
import lombok.Data;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * @ClassName ReportFormTestProduceDTO
 * @Description 质检报表返回
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("质检报表返回")
public class ReportFormTestQualityTestDTO {

    @ApiModelProperty("id")
    private String id;

    @ApiModelProperty("样品编号")
    private String sampleCode;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("炉号")
    private String furnaceName;

    @ApiModelProperty("班次:1一班，2二班，3三班")
    private Integer shift;

    @ApiModelProperty("炉次")
    private Integer furnaceBatch;

    @ApiModelProperty("化验人")
    private String assayPerson;

    @ApiModelProperty("化验时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date assayTime;

    @ApiModelProperty("化验项")
    private List<Map<String,Object>> items;
//    private List<ReportFormTestQualityTestAssayDTO> items;

    private Integer businessType;

    @ApiModelProperty("质检类型")
    private String businessTypeName;

    @ApiModelProperty("磅单号")
    private String machineId;

    @ApiModelProperty("车号")
    private String trainNumber;

    @ApiModelProperty("状态")
    private String assayResult;

    private Integer assayResultInt;

}
