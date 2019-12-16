package com.bee.platform.cloud.si.manufacture.utils;

/**
 * @Author zhanghu
 * @Description: 统一返回类
 * @Date: Create in 10:15 2018/10/31
 * @modified by:
 */
public class ResultUtils {
    public static <T> JsonResult<T> success(Integer code, String message, T data) {
        JsonResult<T> jsonResult = new JsonResult<>(code, message, data);
        return jsonResult;
    }
    public static <T> JsonResult<T> success(Integer code, String message) {
        JsonResult<T> jsonResult = new JsonResult<>();
        jsonResult.setCode(code);
        jsonResult.setMessage(message);
        return jsonResult;
    }
    
    public static <T> JsonResult<T> success(ResultEnum code, String message) {
        JsonResult<T> jsonResult = new JsonResult<>();
        jsonResult.setCode(code.getCode());
        jsonResult.setMessage(message);
        return jsonResult;
    }

    public static <T> JsonResult<T> success(T data) {
        JsonResult<T> jsonResult = new JsonResult<>();
        jsonResult.setCode(ResultEnum.SUCCESS.getCode());
        jsonResult.setMessage(ResultEnum.SUCCESS.getMessage());
        jsonResult.setData(data);
        return jsonResult;
    }

    public static <T> JsonResult<T> success() {
        return success(null);
    }

    public static <T> JsonResult<T> error(Integer code, String message) {
        JsonResult<T> jsonResult = new JsonResult<T>();
        jsonResult.setCode(code);
        jsonResult.setMessage(message);
        return jsonResult;
    }
    public static <T> JsonResult<T> error(Integer code, String message, T data) {
        JsonResult<T> jsonResult = new JsonResult<T>();
        jsonResult.setCode(code);
        jsonResult.setMessage(message);
        jsonResult.setData(data);
        return jsonResult;
    }
}
