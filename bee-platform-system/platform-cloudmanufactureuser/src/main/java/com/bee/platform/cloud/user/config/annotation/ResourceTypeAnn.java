package com.bee.platform.cloud.user.config.annotation;

import com.bee.platform.common.constants.enums.PlatformType;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface ResourceTypeAnn {
    /**
     * 平台类型
     */
    PlatformType platform();
}
