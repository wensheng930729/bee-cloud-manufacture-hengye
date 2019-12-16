package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 通知方式
 * @author: junyang.li
 * @create: 2019-05-06 16:13
 **/
@Getter
public enum NotifyMode {

    WEB(1,"web端"),WEB_SHORT_MESSAGE(2,"web端和短信"),WEB_PC(3,"web和pc端"),WEB_MESSAGE_PC(4,"web端，短信，pc");

    private int key;

    private String value;

    NotifyMode(){

    }

    NotifyMode(int key, String value) {
        this.key = key;
        this.value = value;
    }
}
