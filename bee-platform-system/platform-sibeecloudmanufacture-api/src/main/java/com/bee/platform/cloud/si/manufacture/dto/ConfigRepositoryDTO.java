package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

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
@ApiModel("仓库返回信息")
public class ConfigRepositoryDTO implements Serializable {

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
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    private String name;
    /**
     * 状态1:启用0:停用
     */
    @ApiModelProperty("状态1:启用0:停用")
    private Integer status;
    /**
     * 仓库类别(0 成品 1 原料 2 配料  3五金 4其他)
     */
    @ApiModelProperty("仓库类别(0 成品 1 原料 2 配料  3五金 4其他)")
    private Integer type;





}
