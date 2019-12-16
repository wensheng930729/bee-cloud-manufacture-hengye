package com.bee.platform.cloud.si.manufacture.dto;

import cn.afterturn.easypoi.excel.annotation.Excel;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 报表配置表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("启用的报表配置返回信息")
public class ConfigReportFormsEnableDTO implements Serializable {

    private static final long serialVersionUID = 1L;



    /**
     * 报表名称
     */
    @ApiModelProperty("报表名称")
    private String name;
    /**
     * 报表编号
     */
    @ApiModelProperty("报表编号")
    private String code;


}
