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
 * 仓库档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("保存仓库请求参数")
public class ConfigRepositorySaveRQ implements Serializable {

    private static final long serialVersionUID = 1L;



    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    @NotEmpty(message = "仓库名称不能为空")
    private String name;
    /**
     * 状态1:启用0:停用
     */
    @ApiModelProperty("状态1:启用0:停用")
    @NotNull(message = "状态不能为空")
    private Integer status;
    /**
     * 仓库类别(0 成品 1 原料 2 配料  3五金 4其他)
     */
    @ApiModelProperty("仓库类别(0 成品 1 原料 2 配料  3五金 4其他)")
    @NotNull(message = "仓库类别不能为空")
    private Integer type;





}
