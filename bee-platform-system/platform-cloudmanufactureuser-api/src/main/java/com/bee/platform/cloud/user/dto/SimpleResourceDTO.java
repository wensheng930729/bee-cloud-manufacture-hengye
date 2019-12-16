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
 * @create: 2019-09-26 14:46
 **/
@Setter
@Getter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("web的菜单资源返回的简易数据")
public class SimpleResourceDTO implements Serializable {

    private static final long serialVersionUID = 2379694989803453294L;

    @ApiModelProperty("资源id")
    private Integer resourceId;

    @ApiModelProperty("资源名称")
    private String resourceName;

    @ApiModelProperty("模块id")
    private Integer pid;

    @ApiModelProperty("是否选中 0.未选中，1.已选中")
    private Integer selection;

    @ApiModelProperty("子资源")
    private List<SimpleResourceDTO> children;

    public SimpleResourceDTO(Integer resourceId, String resourceName, Integer pid, Integer selection) {
        this.resourceId = resourceId;
        this.resourceName = resourceName;
        this.pid = pid;
        this.selection = selection;
    }

    public SimpleResourceDTO(Integer resourceId, String resourceName, Integer pid) {
        this.resourceId = resourceId;
        this.resourceName = resourceName;
        this.pid = pid;
    }
}


