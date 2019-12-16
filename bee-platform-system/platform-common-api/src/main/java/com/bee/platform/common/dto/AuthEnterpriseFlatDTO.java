package com.bee.platform.common.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author liang.li
 * @ClassName AuthEnterpriseFlatDTO
 * @Description AuthEnterpriseListDTO企业列表
 * @Date 2019-5-30
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AuthEnterpriseFlatDTO {

    @ApiModelProperty("企业id")
    private Integer value;

    @ApiModelProperty("企业父id")
    private Integer pid;

    @ApiModelProperty("公司全称")
    private String label;

}
