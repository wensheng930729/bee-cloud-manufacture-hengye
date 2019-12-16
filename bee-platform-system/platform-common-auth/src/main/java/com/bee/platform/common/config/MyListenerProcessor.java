package com.bee.platform.common.config;

import com.bee.platform.common.annotation.NotIntercept;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.mvc.condition.PatternsRequestCondition;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import java.lang.reflect.Method;
import java.util.Map;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-08-01 13:24
 **/
@Slf4j
@Configuration
public class MyListenerProcessor implements ApplicationListener<ApplicationReadyEvent> {


    @Value(value = "${server.servlet.context-path}")
    private String contextPath;

    /**
     * @notes: 容器初始化完成之后
     * @Author: junyang.li
     * @Date: 13:24 2019/8/1
     * @return: java.lang.Object
     */
    @Override
    public void onApplicationEvent(ApplicationReadyEvent event) {
        //获得spring 容器
        ConfigurableApplicationContext applicationContext=event.getApplicationContext();
        //获得所有的RequestMapping
        RequestMappingHandlerMapping mapping = applicationContext.getBean(RequestMappingHandlerMapping.class);
        // 获取url与类和方法的对应信息
        Map<RequestMappingInfo, HandlerMethod> map = mapping.getHandlerMethods();
        //遍历
        for (Map.Entry<RequestMappingInfo, HandlerMethod> m : map.entrySet()) {
            //获得对应的方法
            Method handlerMethod=m.getValue().getMethod();
            //获得自定义注解
            NotIntercept notIntercept=handlerMethod.getAnnotation(NotIntercept.class);
            //自定义注解非空
            if(notIntercept!=null){
                //接口信息
                RequestMappingInfo info=m.getKey();
                PatternsRequestCondition p = info.getPatternsCondition();
                for (String url : p.getPatterns()) {
                    AnnotationConstant.requestMappingList.add(contextPath+url);
                }
            }
        }
    }
}
