package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: FreeMarker模板类型
 * @author: junyang.li
 * @create: 2019-05-21 15:10
 **/
@Getter
public enum FreeMarkerType {

    /**
     * 验证码的邮件模板名称
     */
    CODE_EMAIL(1,"code_email.ftl","验证码"),
    /**
     * 重置密码的邮件模板名称
     */
    PASSWORD_EMAIL(2,"password_email.ftl","重置密码");

    private int key;

    private String name;

    private String title;

    FreeMarkerType(){

    }

    FreeMarkerType(int key, String name, String title) {
        this.key = key;
        this.name = name;
        this.title = title;
    }

}
