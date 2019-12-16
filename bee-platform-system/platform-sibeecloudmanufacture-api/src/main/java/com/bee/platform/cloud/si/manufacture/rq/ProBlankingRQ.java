package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "下料参数")
public class ProBlankingRQ implements Serializable {
    private static final long serialVersionUID = 6077909484622608839L;

    @ApiModelProperty("配料id")
    private Long ingredientId;

    @ApiModelProperty("料批名称")
    private String materialName;

    @ApiModelProperty("料批id")
    private Long batchId;

    @ApiModelProperty("成品id")
    private Integer finishProductId;

    @ApiModelProperty("plc设备id")
    private Integer plcId;

    @ApiModelProperty("plc设备名称")
    private String plcName;

    @ApiModelProperty("炉号id")
    @NotNull(message = "炉号不能为空")
    private Integer furnaceId;

    @ApiModelProperty("炉号名称")
    @NotNull(message = "炉号名称不能为空")
    private String furnaceName;

    @ApiModelProperty("班次:1一班，2二班，3三班")
    @NotNull(message = "班次不能为空")
    private Integer shift;

    @ApiModelProperty("数量")
    private BigDecimal num;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("下料时间")
    @NotEmpty(message = "下料时间不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm")
    private Date blankingTime;

    @ApiModelProperty("下料车次")
    private Integer trains;
}
