package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 码表
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel(value = "工厂配置码表DTO")
public class ConfigCodeDTO implements Serializable {

    private static final long serialVersionUID = 1L;


    /**
     * 类型
     */
    @ApiModelProperty(value = "类型")
    private String type;


    /**
     * 编码
     */
    @ApiModelProperty(value = "编码")
    private String code;

    /**
     * 名称
     */
    @ApiModelProperty(value = "名称")
    private String value;

    /**
     * 描述
     */
    @ApiModelProperty(value = "描述")
    private String description;



}
