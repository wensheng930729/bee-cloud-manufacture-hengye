package com.bee.platform.common.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumIsManager
 * @Description 标识用户在该企业下是否是管理员的枚举
 */

public class EnumIsManager {
    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    public enum IsManager{
        not(0,"不是管理员"),yes(1,"是管理员")
        ;

        private Integer key;
        private String value;

    }
}
