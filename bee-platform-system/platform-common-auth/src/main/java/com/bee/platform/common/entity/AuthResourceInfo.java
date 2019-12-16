package com.bee.platform.common.entity;

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
 * @description: 资源相关返回数据
 * @author: junyang.li
 * @create: 2019-07-08 11:30
 **/
@Setter
@Getter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("资源相关返回数据")
public class AuthResourceInfo implements Serializable {

    private static final long serialVersionUID = -5683382242940733219L;

    @ApiModelProperty("资源id")
    private Integer id;

    @ApiModelProperty("资源名称")
    private String name;

    @ApiModelProperty("模块id")
    private Integer pid;

    @ApiModelProperty("模块标识")
    private String subSys;

    @ApiModelProperty("资源类型")
    private String resourceType;

    @ApiModelProperty("菜单图标")
    private String icon;

    @ApiModelProperty("菜单url")
    private String path;

    @ApiModelProperty("是否隐藏0展开1隐藏")
    private Integer isHide;

    @ApiModelProperty("子资源")
    private List<AuthResourceInfo> children;



}
