package com.bee.platform.common.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;


/**
 * @ClassName ResponseResult
 * @Description 封装响应数据的bean
 * @author zhigang.zhou
 * @Date 2018年10月26日 下午1:16:35
 * @version 1.0.0
 * @param <E>
 */
@NoArgsConstructor
@AllArgsConstructor
@Data
@Accessors(chain = true)
public class ResponseResult<E> implements Serializable {

    private static final long serialVersionUID = 3468129702817605150L;

    /**
     * @code : 响应状态码
     */
    private Integer code;

    /**
     * @msg : 该状态码对应的提示信息
     */
    private String message;

    /**
     * @data : 响应数据
     */
    private E object;

    /**
     * @sysToken : 响应数据
     */
    private String sysToken;

    /**
     * 分页信息
     */
    private Page page;

    public ResponseResult(Integer code, String msg) {
        this.code = code;
        this.message = msg;
    }

    public static <E> ResponseResult<E> buildResponseResult(ResCodeEnum resCodeEnum) {
        return new ResponseResult<E>(resCodeEnum.code, resCodeEnum.msg);
    }

    public static <E> ResponseResult<E> buildResponseResult(ResCodeEnum resCodeEnum, E data) {
        return new ResponseResult<E>(resCodeEnum.code, resCodeEnum.msg).setObject(data);
    }

    /*public static <E> ResponseResult<E> buildResponseResult(ResCodeEnum resCodeEnum,String sysToken) {
        return new ResponseResult<E>(resCodeEnum.code, resCodeEnum.msg).setSysToken(sysToken).setObject(null);
    }*/

    public static <E> ResponseResult<E> buildResponseResult(ResCodeEnum resCodeEnum,String sysToken,E object) {
        return new ResponseResult<E>(resCodeEnum.code, resCodeEnum.msg).setSysToken(sysToken).setObject(object);
    }

    public static <E> ResponseResult<E> buildResponseResult(Integer code, String msg) {
        return new ResponseResult<E>(code, msg);
    }

    public static <E> ResponseResult<E> buildResponseResult(Integer code, String msg, E data) {
        return new ResponseResult<E>(code, msg, data, null,null);
    }

    public static <E> ResponseResult<E> buildResponseResult(Integer code, E data, Page page) {
        return new ResponseResult<E>(code, null, data,null, page);
    }

    public static <E> ResponseResult<E> buildResponseResult(ResCodeEnum resCodeEnum, E data, Page page) {
        return new ResponseResult<E>(resCodeEnum.code, resCodeEnum.msg, data,null, page);
    }

    public static <E> ResponseResult<E> buildResponseResult(Integer code, String msg, E data, Page page) {
        return new ResponseResult<E>(code, msg, data,null, page);
    }
    
    public static <E> ResponseResult<E> success(E data) {
        return success(data, null);
    }
    
    public static <E> ResponseResult<E> success(String msg) {
        return buildResponseResult(ResCodeEnum.SUCCESS.code, msg, null, null);
    }
    
    public static <E> ResponseResult<E> success(E data, Page page) {
        return buildResponseResult(ResCodeEnum.SUCCESS.code, null, data, page);
    }
    
    public static <E> ResponseResult<E> fail(ResCodeEnum resCodeEnum) {
        return buildResponseResult(resCodeEnum);
    }
    
    public static <E> ResponseResult<E> fail(String msg) {
        return buildResponseResult(ResCodeEnum.FAILED.code, msg, null, null);
    }
}
