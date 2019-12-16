package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 极验客户端类型
 * @author: junyang.li
 * @create: 2019-04-28 15:35
 **/
@Getter
public enum  GeetestClientType {

    /**
     * web
     */
    WEB("web"),
    /**
     * h5
     */
    H5("h5"),
    /**
     * ios
     */
    IOS("ios"),
    /**
     * android
     */
    ANDROID("android"),
    ;

    private String desc;

    GeetestClientType(){

    }

    GeetestClientType(String desc) {
        this.desc = desc;
    }
}
