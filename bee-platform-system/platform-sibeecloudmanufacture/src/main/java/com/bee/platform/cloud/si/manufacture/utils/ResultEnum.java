package com.bee.platform.cloud.si.manufacture.utils;

public enum ResultEnum {
    SUCCESS(200, "成功"), SYSTEM_INNER_ERROR(500,"系统内部错误"),
    BAD_PARAMETER(400,"参数错误"),FORBIDDEN(403,"无权限");
    private Integer code;
    private String message;

    ResultEnum(Integer code, String message) {
        this.code = code;
        this.message = message;
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
