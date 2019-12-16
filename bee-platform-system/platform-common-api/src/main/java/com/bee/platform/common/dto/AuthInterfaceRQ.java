package com.bee.platform.common.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @Classname AuthInterfaceRQ
 * @Description 接口信息传输对象
 * @Date 2019/06/08 14:21
 * @Author zhigang.zhou
 */
@Getter
@Setter
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("接口信息传输对象")
@ToString
public class AuthInterfaceRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("资源名称")
    @NotEmpty(message = "资源名称不能为空")
    private String name;

    @ApiModelProperty("资源类型")
    @NotNull(message = "资源类型不能为空")
    private String type;

    @ApiModelProperty("子系统标识")
    private String subSys;

    @ApiModelProperty("排序")
    private Integer orderNum;

    @ApiModelProperty("资源地址")
    @NotEmpty(message = "资源地址不能为空")
    private String url;
    
    @ApiModelProperty("接口路由")
    private String beeRouter;

}
