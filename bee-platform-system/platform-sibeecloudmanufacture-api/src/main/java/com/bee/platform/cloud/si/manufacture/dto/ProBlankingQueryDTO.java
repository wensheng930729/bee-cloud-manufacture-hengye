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
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "下料记录返回信息")
public class ProBlankingQueryDTO implements Serializable {
    private static final long serialVersionUID = 216166700660411493L;

    @ApiModelProperty("id")
    private Long id;

    @ApiModelProperty("料批名称")
    private String materialName;

    @ApiModelProperty("plc设备名称")
    private String plcName;

    @ApiModelProperty("炉号id")
    private Integer furnaceId;

    @ApiModelProperty("炉号名称")
    private String furnaceName;

    @ApiModelProperty("班次:1一班，2二班，3三班")
    private Integer shift;

    @ApiModelProperty("数量")
    private BigDecimal num;

    @ApiModelProperty("单位")
    private String unit;

    @ApiModelProperty("下料时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm")
    private Date blankingTime;
}
