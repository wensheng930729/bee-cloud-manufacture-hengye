package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @ClassName ReportFormProductWarehouseDTO
 * @Description 产成品入库报表返回
 * @author jie.zhang
 * @version 1.0.0
 */
@Data
@Accessors(chain = true)
@ApiModel("产成品入库报表返回")
public class ReportFormProductWarehouseDTO {

    @ApiModelProperty("产成品入库id")
    private Integer id;

    @ApiModelProperty("入库时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm")
    private String storageDate;

    @ApiModelProperty("炉号")
    private String furnaceNumber;

    @ApiModelProperty("班次")
    private String scheduling;

    @ApiModelProperty("炉次")
    private String furnaceTimes;

    @ApiModelProperty("产品")
    private String productName;

    @ApiModelProperty("规格")
    private String productSpecName;

    @ApiModelProperty("仓库")
    private String storageName;

    @ApiModelProperty("入库数量")
    private BigDecimal tonWeight;

    @ApiModelProperty("样品编号")
    private String sampleCode;

    @ApiModelProperty("化验项")
    private List<Map<String,Object>> items;
//    private List<ReportFormTestQualityTestAssayDTO> items;

}
