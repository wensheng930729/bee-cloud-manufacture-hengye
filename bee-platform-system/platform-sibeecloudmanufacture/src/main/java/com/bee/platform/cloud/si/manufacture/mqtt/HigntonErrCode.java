package com.bee.platform.cloud.si.manufacture.mqtt;

/**
 * @description: 华辰智通错误码
 * @author: junyang.li
 * @create: 2019-10-10 16:36
 **/
public enum HigntonErrCode {
    /**
     * 华辰智通错误码
     */
    NORMAL(0,"正常的"),
    BREAK_CONNECT(1,"断开连接..."),
    READ_ERROR(2,"读取失败"),
    ;

    private int code;

    private String desc;

    HigntonErrCode(Integer code, String desc) {
        this.code = code;
        this.desc = desc;
    }

    public Integer getCode() {
        return code;
    }

    public String getDesc() {
        return desc;
    }
}
