package com.bee.platform.common.annotation;


import java.lang.annotation.*;

/**
 * @notes: 用于标记哪些接口不被拦截器拦截器
 * @Author: junyang.li
 * @Date: 13:18 2019/8/1
 * @return:
 */
@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface NotIntercept {

    String value()  default "";
}
