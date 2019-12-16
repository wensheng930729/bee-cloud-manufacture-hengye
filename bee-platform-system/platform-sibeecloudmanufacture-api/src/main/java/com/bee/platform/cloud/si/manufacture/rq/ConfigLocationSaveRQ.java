package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * <p>
 * 物流地点管理表
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "物流地点管理保存请求参数")
public class ConfigLocationSaveRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 地点名称
     */
    @ApiModelProperty("地点名称")
    @NotEmpty(message = "地点名称不能为空")
    private String name;
    /**
     * 状态 （1 启用 0 禁用）
     */
    @ApiModelProperty("状态 （1 启用 0 禁用）")
    @NotNull(message = "状态不能为空")
    private Integer status;
   

}
