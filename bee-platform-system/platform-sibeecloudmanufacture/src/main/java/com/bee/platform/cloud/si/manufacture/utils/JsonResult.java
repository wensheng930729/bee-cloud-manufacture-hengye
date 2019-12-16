package com.bee.platform.cloud.si.manufacture.utils;

import java.io.Serializable;

/**
 * 统一返回值处理
 * @param <T>
 */
public class JsonResult<T> implements Serializable {
    private Integer code;
    private String message;
    private T data;

    public JsonResult() {
    }

    public JsonResult(Integer code, String message, T data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    public JsonResult(Integer code, String message) {
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

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }


}
