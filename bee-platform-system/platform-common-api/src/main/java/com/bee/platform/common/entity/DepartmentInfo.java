package com.bee.platform.common.entity;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @description: 部门信息
 * @author: junyang.li
 * @create: 2019-03-06 10:09
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("部门信息")
public class DepartmentInfo implements Serializable {

    private static final long serialVersionUID = -6489532006588327720L;

    @ApiModelProperty("部门Id")
    private Integer departmentId;

    @ApiModelProperty("部门名称")
    private String departmentName;

    @ApiModelProperty("企业id")
    private Integer orgId;

    @ApiModelProperty("职能描述")
    private String description;

    @ApiModelProperty("职位id")
    private Integer postId;

    @ApiModelProperty("职位名称")
    private String postName;

}
