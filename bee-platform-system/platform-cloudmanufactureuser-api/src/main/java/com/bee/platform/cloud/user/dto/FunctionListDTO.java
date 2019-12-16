package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-09-20 16:29
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("功能列表")
public class FunctionListDTO implements Serializable {

    private static final long serialVersionUID = 8775496720339801223L;

    @ApiModelProperty("资源id")
    private Integer resourceId;

    @ApiModelProperty("资源名称")
    private String resourceName;

    @ApiModelProperty("是否选中 0.未选中，1.已选中")
    private Integer selection;

    public FunctionListDTO(Integer resourceId, String resourceName, Integer selection) {
        this.resourceId = resourceId;
        this.resourceName = resourceName;
        this.selection = selection;
    }
}
