package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-10-11 14:15
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class FactoryGatewayParam implements Serializable {

    private static final long serialVersionUID = -87792519359403511L;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 设备名称
     */
    private String  hcGatewayName;
    /**
     * 数据是否禁用
     */
    private Integer status;

    public FactoryGatewayParam(Integer factoryId) {
        this.factoryId = factoryId;
    }

    public FactoryGatewayParam(Integer factoryId, String hcGatewayName, Integer status) {
        this.factoryId = factoryId;
        this.hcGatewayName = hcGatewayName;
        this.status = status;
    }

    public FactoryGatewayParam(Integer factoryId, Integer status) {
        this.factoryId = factoryId;
        this.status = status;
    }
}
