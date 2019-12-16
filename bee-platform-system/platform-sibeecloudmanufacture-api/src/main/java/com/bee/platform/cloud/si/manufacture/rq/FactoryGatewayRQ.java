package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @description: 工厂网关新增参数
 * @author: junyang.li
 * @create: 2019-10-11 20:32
 **/
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "工厂网关新增参数")
public class FactoryGatewayRQ implements Serializable {

    private static final long serialVersionUID = -3510904423068649382L;

    @ApiModelProperty("华辰智通网关id")
    @NotEmpty(message = "网关id不能为空")
    @Length(max = 40,message = "网关id限制40个字符")
    private String hcGatewayId;
    /**
     *
     */
    @ApiModelProperty("设备名称")
    @NotEmpty(message = "设备名称不能为空")
    @Length(max = 50,message = "设备名称限制50个字符")
    private String hcGatewayName;

    @ApiModelProperty("启用状态")
    @NotNull(message = "启用状态不能为空")
    @Range(max = 1,message = "只能选择启用或禁用")
    private Integer status;
}
