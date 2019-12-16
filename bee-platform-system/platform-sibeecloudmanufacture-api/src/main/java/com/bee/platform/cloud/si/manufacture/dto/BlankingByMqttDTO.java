package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @description: 通过MQTT 实时更新下料数据
 * @author: junyang.li
 * @create: 2019-10-12 16:59
 **/
@Data
@Accessors(chain = true)
@ApiModel("通过MQTT 实时更新下料数据")
public class BlankingByMqttDTO implements Serializable {

    private static final long serialVersionUID = 1422693246566705785L;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("实时数据")
    private BigDecimal num;

    public BlankingByMqttDTO() {
    }

    public BlankingByMqttDTO(Integer productId, Integer productSpecId, BigDecimal num) {
        this.productId = productId;
        this.productSpecId = productSpecId;
        this.num = num;
    }
}
