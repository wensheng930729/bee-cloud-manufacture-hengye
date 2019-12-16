package com.bee.platform.cloud.si.manufacture.rq;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;

/**
 * <p>
 * 电表档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "电表管理保存请求参数")
public class ConfigAmmeterSaveRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 电表名称
     */
    @ApiModelProperty("电表名称")
    @NotEmpty(message = "电表名称不能为空")
    private String name;
    /**
     * 电表编号
     */
    @ApiModelProperty("电表编号")
    @NotEmpty(message = "电表编号不能为空")
    private String code;

    /**
     * 电表类型（0三相三线 1三相四线）
     */
    @ApiModelProperty("电表类型（0三相三线 1三相四线）")
    private Integer meterType;
    /**
     * 用电类型(0炉变电 1动力电)
     */
    @ApiModelProperty("用电类型(0炉变电 1动力电)")
    private Integer electricityType;
    /**
     * 协议类型（0 130协议 1 1376.1协议）
     */
    @ApiModelProperty("协议类型（0 130协议 1 1376.1协议）")
    private Integer protocolType;
    /**
     * sim卡号
     */
    @ApiModelProperty("sim卡号")
    private String simNumber;
    /**
     * 电压倍率
     */
    @ApiModelProperty("电压倍率")
    private Integer voltageRate;
    /**
     * 电流倍率
     */
    @ApiModelProperty("电流倍率")
    private Integer currentRate;






}
