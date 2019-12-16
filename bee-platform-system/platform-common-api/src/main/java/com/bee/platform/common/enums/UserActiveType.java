package com.bee.platform.common.enums;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @Classname UserActiveType
 * @Description 用户激活类型
 * @Date 2019/5/24 10:01
 * @Author xin.huang
 */
@Getter
@NoArgsConstructor
public enum UserActiveType {
    /**
     * 平台注册
     */
    PLATFORM_REGISTER(0),
    /**
     * 平台添加
     */
    PLATFORM_ADD(1);

    private Integer key;

    UserActiveType(Integer key){
        this.key=key;
    }
}
