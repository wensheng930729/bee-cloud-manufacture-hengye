package com.bee.platform.cloud.user.dto;

import lombok.*;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-10-22 15:20
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class AuthRoleParam  implements Serializable {
    private static final long serialVersionUID = -6868787947445534914L;
    /**
     * 角色名称
     */
    private String roleName;
    /**
     * 角色类型
     */
    private String[] roleTypes;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 角色id
     */
    private Integer roleId;

    public AuthRoleParam(String roleName, String[] roleTypes, Integer enterpriseId) {
        this.roleName = roleName;
        this.roleTypes = roleTypes;
        this.enterpriseId = enterpriseId;
    }
}
