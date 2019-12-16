package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 工厂网关列表查询接口
 * @author: junyang.li
 * @create: 2019-10-11 20:13
 **/
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "工厂网关列表查询接口")
public class FactoryGatewayParamRQ implements Serializable {
    private static final long serialVersionUID = -6997998328165845347L;

    @ApiModelProperty("设备名称")
    private String  hcGatewayName;

    @ApiModelProperty("是否有效")
    private Integer status;
}
