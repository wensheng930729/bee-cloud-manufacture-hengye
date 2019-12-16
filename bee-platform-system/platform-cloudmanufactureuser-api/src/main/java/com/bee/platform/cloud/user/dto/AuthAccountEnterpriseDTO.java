package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 上下游账号所在公司信息
 * </p>
 *
 * @author junyang.li123
 * @since 2019-10-18
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("上下游账号所在公司返回信息")
public class AuthAccountEnterpriseDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 账号id
     */
    @ApiModelProperty("账号id")
    private Integer accountId;
    /**
     * 所在公司id
     */
    @ApiModelProperty("所在公司id")
    private Integer enterpriseId;
    /**
     * 所在公司名称
     */
    @ApiModelProperty("所在公司名称")
    private String name;
    /**
     * 职务
     */
    @ApiModelProperty("职务")
    private String job;
    /**
     * 默认公司（0非默认 1默认）
     */
    @ApiModelProperty("默认公司（0非默认 1默认）")
    private Integer type;


}
