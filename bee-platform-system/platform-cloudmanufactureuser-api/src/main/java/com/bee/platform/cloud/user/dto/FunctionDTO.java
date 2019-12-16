package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-09-20 16:37
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("功能相关数据")
public class FunctionDTO implements Serializable {

    private static final long serialVersionUID = 4445930673143021887L;

    @ApiModelProperty("APP模块权限")
    private List<FunctionListDTO> app;

    @ApiModelProperty("web模块权限")
    private List<FunctionListDTO> web;

    @ApiModelProperty("APP模块权限")
    private List<FunctionListDTO> bi;
}
