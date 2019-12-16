package com.bee.platform.cloud.si.manufacture.dto;

import cn.afterturn.easypoi.excel.annotation.Excel;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

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
@ApiModel("报表配置树返回信息")
public class ConfigReportFormsTreeDTO implements Serializable {

    private static final long serialVersionUID = 1L;
    @ApiModelProperty("id")
    private Integer id;

    /**
     * 父id
     */
    @ApiModelProperty("父id")
    private Integer pid;

    /**
     * 等级
     */
    @ApiModelProperty("等级")
    private Integer level;

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
    /**
     * 状态:1-启用,0-禁用
     */
    @ApiModelProperty("状态:1-启用,0-禁用")
    private Integer status;

    /**
     * 类型（0默认类型，1企业自定义类型）
     */
    @Excel(name = "类型（0默认类型，1企业自定义类型）", orderNum = "6", type = 10, width = 20)
    @ApiModelProperty("类型（0默认类型，1企业自定义类型）")
    private Integer type;

    @ApiModelProperty("子报表配置")
    private List<ConfigReportFormsTreeDTO> children;

}
