package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * <p>
 * 称重设备档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("修改称重设备返回信息")
public class ConfigWeighDeviceUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;

    /**
     * 称重设备名称
     */
    @ApiModelProperty("称重设备名称")
    @NotEmpty(message = "称重设备名称不能为空")
    private String name;
    /**
     * 状态 1启用 0未启用
     */
    @ApiModelProperty("状态 1启用 0未启用")
    @NotNull(message = "状态不能为空")
    private Integer status;
    /**
     * 称重设备类型
     */
    @ApiModelProperty("称重设备类型(0 地磅 1行车称)")
    @NotNull(message = "称重设备类型不能为空")
    private Integer type;
    /**
     * 设备id
     */
    @ApiModelProperty("设备id")
    @NotEmpty(message = "设备编号不能为空")
    private String deviceId;





}
