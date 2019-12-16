package com.bee.platform.common.exception;

/**
 * @ClassName GlobalExceptionHandler
 * @Description 无法找到相应的数据的自定义异常
 * @author zhigang.zhou
 * @Date 2018年10月26日 下午1:16:35
 * @version 1.0.0
 */
public class NoFoundException extends Exception {

	private static final long serialVersionUID = -5955607821816077172L;

	public NoFoundException(String errorInfo) {
		super(errorInfo);
	}

}
