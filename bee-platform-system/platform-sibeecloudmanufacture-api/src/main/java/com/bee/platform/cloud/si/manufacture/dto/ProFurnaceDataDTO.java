package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.Map;

/**
 * @author xin.huang
 * @description
 * @date 2019/10/25
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "矿热炉相关数据返回信息")
public class ProFurnaceDataDTO implements Serializable {
    private static final long serialVersionUID = -197299168482285831L;

    @ApiModelProperty(value = "总量")
    private BigDecimal totalNum;
    ;
    @ApiModelProperty(value = "矿热炉数据总量统计")
    private Collection<Map<String, Object>> furnaces;

}
