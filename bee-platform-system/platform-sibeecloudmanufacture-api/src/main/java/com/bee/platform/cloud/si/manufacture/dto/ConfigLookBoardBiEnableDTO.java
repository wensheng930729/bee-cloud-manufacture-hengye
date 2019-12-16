package com.bee.platform.cloud.si.manufacture.dto;

import cn.afterturn.easypoi.excel.annotation.Excel;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 看板BI配置表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "启用的看板BI配置返回信息")
public class ConfigLookBoardBiEnableDTO implements Serializable {

    private static final long serialVersionUID = 1L;


    /**
     * 看板名称
     */
    @ApiModelProperty("看板名称")
    private String name;
    /**
     * 看板编号
     */
    @ApiModelProperty("看板编号")
    private String code;



}
