package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel("称重设备返回信息")
public class ConfigWeighDeviceDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 所属企业id
     */
    @ApiModelProperty("所属企业id")
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @ApiModelProperty("工厂id")
    private Integer factoryId;
    /**
     * 称重设备名称
     */
    @ApiModelProperty("称重设备名称")
    private String name;
    /**
     * 状态 1启用 0未启用
     */
    @ApiModelProperty("状态 1启用 0未启用")
    private Integer status;
    /**
     * 称重设备类型(0 地磅 1行车称)
     */
    @ApiModelProperty("称重设备类型")
    private Integer type;
    /**
     * 设备id
     */
    @ApiModelProperty("设备id")
    private String deviceId;





}
