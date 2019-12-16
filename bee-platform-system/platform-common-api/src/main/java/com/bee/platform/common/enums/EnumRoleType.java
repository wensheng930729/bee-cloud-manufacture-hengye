 package com.bee.platform.common.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author Raphael.dq
 * @date 2019/05/22
 */
@Getter
@AllArgsConstructor
public enum EnumRoleType {
    
    /**
     * 应用类角色
     */
    APPLICATION("application"),
    /**
     * 功能类角色
     */
    FUNCTION_ONE("function_one"),
    /**
     * 功能类角色
     */
    FUNCTION_TWO("function_two"),
    /**
     * 自定义角色，base角色的聚合
     */
    CUSTOM("custom"),
    /**
     * 基础类角色,底层功能的增删查看
     */
    BASE("base"),
    /**
     * 企业管理员
     */
    ENTERPRISE_ADMIN("enterprise_admin"),
    
    /**
     * 超级管理员
     */
    SUPER_ADMIN("super_admin");
    
    
    private String code;

}
