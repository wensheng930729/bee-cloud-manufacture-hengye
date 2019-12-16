package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author xin.huang
 * @description 矿热炉产量数据返回信息
 * @date 2019/10/21
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "矿热炉产量数据返回信息")
public class ProFurnaceProductionDTO implements Serializable {
    private static final long serialVersionUID = 2380688499230252641L;

    @ApiModelProperty(value = "总产量")
    private BigDecimal totalNum;

    @ApiModelProperty(value = "矿热炉产量数据明细返回信息")
    private List<ProFurnaceProductionDetailDTO> details;
}
