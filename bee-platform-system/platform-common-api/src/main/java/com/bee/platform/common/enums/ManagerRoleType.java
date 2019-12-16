package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 管理后台角色类型  Basic
 * @author: junyang.li
 * @create: 2019-04-29 15:58
 **/
@Getter
public enum  ManagerRoleType {
    /**
     * 超级管理员
     */
    SUPPER(1,"超级管理员"),
    /**
     * 基础角色
     */
    BASIC(2,"基础角色"),
    /**
     * 普通角色
     */
    COMMON(3,"普通角色"),
    /**
     * 基础角色组
     */
    BASIC_ROLE_GROUP(4,"基础角色组"),

    ;

    private int key;

    private String value;

    ManagerRoleType() {

    }

    ManagerRoleType(int key, String value) {
        this.key = key;
        this.value = value;
    }
}
