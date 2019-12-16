package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

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
@ApiModel(value = "物流地点管理返回信息")
public class ConfigLocationDTO implements Serializable {

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
     * 地点名称
     */
    @ApiModelProperty("地点名称")
    private String name;
    /**
     * 状态 （1 启用 0 禁用）
     */
    @ApiModelProperty("状态 （1 启用 0 禁用）")
    private Integer status;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;

}
