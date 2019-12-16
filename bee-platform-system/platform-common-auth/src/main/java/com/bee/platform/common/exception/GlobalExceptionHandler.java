package com.bee.platform.common.exception;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.alibaba.fastjson.JSON;
import com.bee.platform.common.config.SentryConfig;
import io.sentry.SentryClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.BindException;
import org.springframework.validation.ObjectError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import lombok.extern.slf4j.Slf4j;

/**
 * @ClassName GlobalExceptionHandler
 * @Description 统一业务异常类，代码中抛出的异常都需要进行相应封装处理
 * @author zhigang.zhou
 * @Date 2018年10月26日 下午1:16:35
 * @version 1.0.0
 */
@Slf4j
@CrossOrigin
@RestControllerAdvice
public class GlobalExceptionHandler {

	@ExceptionHandler
	public ResponseResult<?> processException(Exception ex, HttpServletRequest request, HttpServletResponse response) {
		if (ex instanceof MissingServletRequestParameterException) {
			log.info("MissingServletRequestParameterException={}", ex);
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
		}
		if (ex instanceof NoFoundException) {
			log.error("NoFoundException={}", ex);
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
		}

		// 统一参数校验错误转换
		if (ex instanceof MethodArgumentNotValidException) {
			log.info("MethodArgumentNotValidException=", ex);
			List<ObjectError> errors = ((MethodArgumentNotValidException) ex).getBindingResult().getAllErrors();
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER.getCode(),errors.isEmpty() ? "未知参数错误！" : errors.get(0).getDefaultMessage());
		}
		if (ex instanceof BindException) {
			log.info("BindException={}", ex);
			List<ObjectError> errors = ((BindException) ex).getBindingResult().getAllErrors();
			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER).setObject(errors.isEmpty() ? "未知参数错误！" : errors.get(0).getDefaultMessage());
		}
		// 统一业务异常
		if (ex instanceof BusinessException) {
			BusinessException exception = (BusinessException) ex;
			log.info(String.format("UserBusinessException: code=%d, message=%s",
					exception.getCode(), exception.getMessage()));
			return ResponseResult.buildResponseResult(exception.getFrontCode(),exception.getMessage());
		} else if (ex instanceof IllegalArgumentException) {
		    log.info(String.format("IllegalArgumentException: message=%s", ex.getMessage()));
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, ex.getMessage());
		}
		/*SentryClient sentryClient=SentryConfig.sentryClient;
		if(sentryClient!=null){
			sentryClient.sendMessage(JSON.toJSONString(ex));
		}*/
		log.info("全局异常ex={}", ex);
		return ResponseResult.buildResponseResult(ResCodeEnum.BUSY_SYSTEM);
	}
	
}
