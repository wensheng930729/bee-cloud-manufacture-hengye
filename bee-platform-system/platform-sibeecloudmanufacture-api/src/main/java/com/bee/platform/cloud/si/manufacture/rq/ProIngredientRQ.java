package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/25
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "配料参数")
public class ProIngredientRQ implements Serializable {
    private static final long serialVersionUID = -7186527240913672686L;

    @ApiModelProperty("料批id")
    private Long batchId;

    @ApiModelProperty("料批名称")
    private String materialName;

    @ApiModelProperty("成品id")
    private Integer finishProductId;

    @ApiModelProperty("plc设备id")
    private Integer plcId;

    @ApiModelProperty("是否PLC设备配料：0否，1是")
    private Integer type;

    @ApiModelProperty("是否PLC定时保存：0否，1是")
    private Integer timeSave;

    @ApiModelProperty("下料状态，1已下料，0未下料")
    private Integer blankingStatus;

    @ApiModelProperty("配料明细参数")
    private List<ProIngredientDetailRQ> ingredientDetailList;
}
