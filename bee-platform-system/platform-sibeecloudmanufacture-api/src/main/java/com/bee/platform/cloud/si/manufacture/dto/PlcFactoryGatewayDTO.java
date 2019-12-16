package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * plc 工厂和华辰智通网关的配置
 * </p>
 *
 * @author MP123
 * @since 2019-10-11
 */
@Data
@Accessors(chain = true)
public class PlcFactoryGatewayDTO implements Serializable{
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("华辰智通网关id")
    private String hcGatewayId;

    @ApiModelProperty("设备名称")
    private String  hcGatewayName;

    @ApiModelProperty("是否被禁用 0 禁用 ，1 未禁用")
    private Integer status;

    public PlcFactoryGatewayDTO() {
    }

    public PlcFactoryGatewayDTO(String hcGatewayId, String hcGatewayName, Integer status) {
        this.hcGatewayId = hcGatewayId;
        this.hcGatewayName = hcGatewayName;
        this.status = status;
    }
}
