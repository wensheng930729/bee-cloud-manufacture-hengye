package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 设备巡检
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("设备巡检返回信息")
public class ConfigDeviceInspectionDTO implements Serializable {

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
     * 设备id
     */
    @ApiModelProperty("设备id")
    private Integer deviceId;
    /**
     * 设备名称
     */
    @ApiModelProperty("设备名称")
    private String name;
    /**
     * 巡检项目
     */
    @ApiModelProperty("巡检项目")
    private String inspectionItem;
    /**
     * 设备编号
     */
    @ApiModelProperty("设备编号")
    private String code;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;




}
