package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 电表档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "电表管理返回DTO")
public class ConfigAmmeterDTO implements Serializable {

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
     * 工厂名称
     */
    @ApiModelProperty("工厂名称")
    private String factoryName;
    /**
     * 电表名称
     */
    @ApiModelProperty("电表名称")
    private String name;
    /**
     * 电表编号
     */
    @ApiModelProperty("电表编号")
    private String code;
    /**
     * 状态 1启用 0未启用
     */
    @ApiModelProperty("状态 1启用 0未启用")
    private Integer status;
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

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTime;

}
