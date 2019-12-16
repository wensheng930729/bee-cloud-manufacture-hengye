package com.bee.platform.common.enums;

/**
 * @Classname UserType
 * @Description 用户类型
 * @Date 2019/5/24 10:03
 * @Author xin.huang
 */

import lombok.Getter;
import lombok.NoArgsConstructor;
@Getter
@NoArgsConstructor
public enum UserType {
    /**
     * 管理员用户
     */
    ADMIN_USER(1),
    /**
     * 普通用户
     */
    PLAIN_USER(2),
    /**
     * 客户
     */
    CUSTOMER(3),
    ;
    private Integer key;

    UserType(Integer key){
        this.key=key;
    }
}
