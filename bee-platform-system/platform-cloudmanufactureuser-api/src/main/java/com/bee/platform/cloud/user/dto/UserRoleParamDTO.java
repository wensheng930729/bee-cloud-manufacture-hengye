package com.bee.platform.cloud.user.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description: 用户角色查询参数
 * @author: junyang.li
 * @create: 2019-09-19 13:34
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class UserRoleParamDTO implements Serializable {

    private static final long serialVersionUID = 1566380470263718951L;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 子应用标识
     */
    private String cloudMafType;


    public UserRoleParamDTO(Integer userId, Integer enterpriseId) {
        this.userId = userId;
        this.enterpriseId = enterpriseId;
    }

    public UserRoleParamDTO(Integer userId, Integer enterpriseId, String cloudMafType) {
        this.userId = userId;
        this.enterpriseId = enterpriseId;
        this.cloudMafType = cloudMafType;
    }
}
