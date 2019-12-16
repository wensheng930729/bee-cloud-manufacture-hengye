package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/10/12
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "配料明细统计返回信息")
public class ProIngredientStatisticDTO implements Serializable {
    private static final long serialVersionUID = 5656418557014091757L;

    @ApiModelProperty("料批id")
    private Long batchId;

    @ApiModelProperty("料批名称")
    private String materialName;

    @ApiModelProperty("成品id")
    private Integer finishProductId;

    @ApiModelProperty("plc设备id")
    private Integer plcId;

    @ApiModelProperty("plc设备名称")
    private String plcName;

    @ApiModelProperty("创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    @ApiModelProperty("下料时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm")
    private Date blankingTime;

    @ApiModelProperty(value = "配料明细返回信息")
    private List<ProIngredientDetailDTO> ingredientDetailList;
}
