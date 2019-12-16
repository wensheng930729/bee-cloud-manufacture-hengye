package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @Description 产量分析报表查询结果
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/10/9 9:53
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "产量分析报表查询结果")
public class ReportFormYieldAnalysisDTO implements Serializable {

    private static final long serialVersionUID = -834596346831095790L;

    @ApiModelProperty("样品编号")
    private String sampleCode;

    @ApiModelProperty("班次编码")
    private Integer shiftCode;

    @ApiModelProperty("班次:1一班，2二班，3三班")
    private String shift;

    @ApiModelProperty("班次时间")
    private String shiftTime;

    @ApiModelProperty("炉号id")
    private Integer furnaceId;

    @ApiModelProperty("炉号")
    private String furnaceName;

    @ApiModelProperty("出炉批次编号")
    private Integer furnaceBatchCode;

    @ApiModelProperty("出炉批次")
    private String furnaceBatch;

    @ApiModelProperty("产品标号")
    private String productName;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("总数量")
    private BigDecimal totalAmount;

}
