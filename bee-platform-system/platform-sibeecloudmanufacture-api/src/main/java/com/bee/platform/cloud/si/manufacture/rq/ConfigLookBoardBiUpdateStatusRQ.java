package com.bee.platform.cloud.si.manufacture.rq;

import cn.afterturn.easypoi.excel.annotation.Excel;
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
 * 看板BI配置表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "看板BI配置修改状态请求参数")
public class ConfigLookBoardBiUpdateStatusRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;

    /**
     * 看板名称
     */
    @ApiModelProperty("看板名称")
    private String name;
    /**
     * 看板编号
     */
    @ApiModelProperty("看板编号")
    @NotEmpty(message = "看板编号不能为空")
    private String code;



}
