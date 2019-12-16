package com.bee.platform.common.interceptor;


import com.alibaba.fastjson.JSON;
import com.bee.platform.common.config.AnnotationConstant;
import com.bee.platform.common.config.AuthConstants;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


/**
 * notes  认证拦截器
 * author junyyang.li
 * date 2018/11/2 0002 13:41
 **/
@Slf4j
@CrossOrigin(origins = "*")
@Component
public class AuthorInterceptor implements HandlerInterceptor {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
        //获得请求的方式
        String method=request.getMethod();
        if(AuthConstants.OPTIONS.equalsIgnoreCase(method)){
            log.info("options 请求直接放行");
            return  true;
        }
        //获得请求的uri
        String uri=request.getRequestURI();
        //被标注为放行的接口
        if(AnnotationConstant.requestMappingList.contains(uri)){
            return true;
        }
        //从缓存中获得用户信息
        String sysToken= WebUtils.getParam(AuthConstants.SYS_TOKEN,request);
        try {
            userInfoUtils.getUserInfo(sysToken);
            return true;
        }catch (BusinessException e){
            log.info("拦截器中无法获取用户信息,当前用户Token={}，请求的接口是:{}",sysToken,method);
            this.result(response,ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
       return false;
    }

    private void result(HttpServletResponse response, ResCodeEnum resCodeEnum)throws Exception{
        ResponseResult<Object> rest= ResponseResult.buildResponseResult(resCodeEnum.getCode(),resCodeEnum.getMsg());
        String data= JSON.toJSONString(rest);
        response.setCharacterEncoding(AuthConstants.UTF8);
        response.setHeader("Content-Type", AuthConstants.CONTENT_TYPE);
        response.getWriter().write(data);
    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response, Object handler, ModelAndView modelAndView)
            throws Exception {

    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex)
            throws Exception {

    }
}
