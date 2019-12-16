package com.bee.platform.common.exception;


import com.bee.platform.common.entity.ResCodeEnum;
import lombok.Getter;

import java.text.MessageFormat;


/**
 * @ClassName BusinessException
 * @Description 运营管理系统统一业务异常类，代码中抛出的异常都需要进行相应封装处理
 * @author zhigang.zhou
 * @Date 2018年10月26日 下午1:16:35
 * @version 1.0.0
 */
@Getter
public class BusinessException extends RuntimeException {

    private static final long serialVersionUID = 2580622941319342399L;

    private Integer code;

    private Integer frontCode;

    private String message;

    public BusinessException(ResCodeEnum frontEnum, ExceptionMessageEnum exceptionMessageEnum, Object ... args) {
        super(null, null, true, false);
        this.frontCode = frontEnum.code;
        this.code = exceptionMessageEnum.getCode();
        this.message = MessageFormat.format(exceptionMessageEnum.getMessage(), args).replaceAll("\\{\\d+\\}", "");
    }

    public BusinessException(ResCodeEnum frontEnum, String msg) {
        super(null, null, true, false);
        this.frontCode = frontEnum.code;
        this.message = MessageFormat.format(msg, null).replaceAll("\\{\\d+\\}", "");
    }
}
