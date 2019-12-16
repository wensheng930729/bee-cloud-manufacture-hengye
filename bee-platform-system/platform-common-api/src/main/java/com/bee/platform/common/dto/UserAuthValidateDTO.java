 package com.bee.platform.common.dto;

import java.io.Serializable;

import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class UserAuthValidateDTO implements Serializable{
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    /**
     * 请求uri
     */
    private String uri;
    /**
     * 请求方法
     */
    private String method;
    /**
     * 用户token
     */
    private String token;
    
    /**
     * 系统标识
     */
    private String platform;
    

}
