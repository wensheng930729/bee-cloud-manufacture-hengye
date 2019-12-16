package com.bee.platform.common.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @ClassName AuthResourceDetailDTO
 * @Description 资源详细信息
 * @author jie.chen
 * @Date 2019/5/20$ 15:07$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("资源详细信息")
public class AuthResourceDetailDTO implements Serializable {

    private static final long serialVersionUID = 6667253386574646484L;

    @ApiModelProperty("菜单id")
    private Integer id;

    @ApiModelProperty("父id")
    private Integer pid;

    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("菜单名称")
    private String name;

    /*@ApiModelProperty("菜单类型")
    private Integer type;*/

    @ApiModelProperty("菜单图标")
    private String icon;

    @ApiModelProperty("菜单url")
    private String path;

    @ApiModelProperty("菜单component")
    private String component;

    /*@ApiModelProperty("菜单序号")
    private Integer orderNum;*/

    @ApiModelProperty("是否开通")
    private boolean openStatu;

    /*@ApiModelProperty("是否隐藏0展开1隐藏")
    private Integer hide;*/

    @ApiModelProperty("是否隐藏")
    private boolean hideChildrenMenu;

    /*@ApiModelProperty("权限")
    private boolean authority;*/

    @ApiModelProperty("创建时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    @ApiModelProperty("修改时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updateTime;

    @ApiModelProperty("子菜单")
    List<AuthResourceDetailDTO> routes;
}
