package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * PLC设备档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("保存PLC设备请求参数")
public class ConfigPlcDeviceSaveRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * PLC名称
     */
    @ApiModelProperty("PLC名称")
    @NotEmpty(message = "PLC名称不能为空")
    private String name;
    /**
     * 品牌
     */
    @ApiModelProperty("品牌")
    @NotEmpty(message = "品牌不能为空")
    private String brand;
    /**
     * 状态 1启用 0未启用
     */
    @ApiModelProperty("状态 1启用 0未启用")
    @NotNull(message = "状态不能为空")
    private Integer status;
    /**
     * 型号
     */
    @ApiModelProperty("型号")
    @NotEmpty(message = "型号不能为空")
    private String type;


    @ApiModelProperty("漏斗配置相关数据")
    private List<PlcFieldConfigRQ> fieldConfig;


}
