package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel("PLC设备档案返回信息")
public class ConfigPlcDeviceDTO implements Serializable {

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
     * PLC名称
     */
    @ApiModelProperty("PLC名称")
    private String name;
    /**
     * 品牌
     */
    @ApiModelProperty("品牌")
    private String brand;
    /**
     * 状态 1启用 0未启用
     */
    @ApiModelProperty("状态 1启用 0未启用")
    private Integer status;
    /**
     * 型号
     */
    @ApiModelProperty("型号")
    private String type;

    @ApiModelProperty("下料漏斗列表")
    private List<PlcFieldConfigDTO> fieldConfigs;


}
