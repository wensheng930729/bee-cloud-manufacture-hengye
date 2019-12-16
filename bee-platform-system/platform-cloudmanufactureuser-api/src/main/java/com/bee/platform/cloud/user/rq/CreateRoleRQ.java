package com.bee.platform.cloud.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

/**
 * @description: 创建角色传入参数
 * @author: junyang.li
 * @create: 2019-09-20 17:04
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("创建角色传入参数")
public class CreateRoleRQ implements Serializable {

    private static final long serialVersionUID = -8394734607785723345L;

    @ApiModelProperty("角色id  为空新增，不为空则编辑")
    private Integer roleId;

    @ApiModelProperty("角色名称")
    @NotEmpty(message = "角色名称不能为空")
    @Length(max = 15,message = "角色名称限制15个字符")
    private String roleName;

    @ApiModelProperty("描述")
    @Length(max = 60,message = "描述限制60个字符")
    private String describe;

    @ApiModelProperty("app功能id")
    private List<Integer> app;

    @ApiModelProperty("web端功能id")
    private List<Integer> web;

    @ApiModelProperty("bi端功能id")
    private List<Integer> bi;
}
