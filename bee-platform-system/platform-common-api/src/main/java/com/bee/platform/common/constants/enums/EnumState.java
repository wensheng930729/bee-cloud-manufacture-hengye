package com.bee.platform.common.constants.enums;

import lombok.Getter;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-09-25 15:16
 **/
@Getter
public enum  EnumState {
    /**
     * 账号状态枚举
     */
    PROHIBIT(0,"已禁用"),

    ENABLE(1,"已启用");

    private int key;

    private String value;


    public static String getValue(int key){
        return PROHIBIT.key==key?PROHIBIT.value:ENABLE.value;
    }

    EnumState(int key, String value) {
        this.key = key;
        this.value = value;
    }
}
