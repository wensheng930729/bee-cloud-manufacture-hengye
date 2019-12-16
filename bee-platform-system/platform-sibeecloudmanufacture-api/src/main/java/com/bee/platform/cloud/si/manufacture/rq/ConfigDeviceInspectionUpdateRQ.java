package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
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
@ApiModel("修改设备巡检返请求参数")
public class ConfigDeviceInspectionUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;
    /**
     * 设备id
     */
    @ApiModelProperty("设备id")
    @NotNull(message = "设备id不能为空")
    private Integer deviceId;
    /**
     * 设备名称
     */
    @ApiModelProperty("设备名称")
    @NotEmpty(message = "设备名称不能为空")
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
    @NotEmpty(message = "设备编号不能为空")
    private String code;






}
