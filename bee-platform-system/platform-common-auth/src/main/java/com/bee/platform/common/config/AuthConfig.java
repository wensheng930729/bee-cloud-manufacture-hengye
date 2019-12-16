 package com.bee.platform.common.config;

import java.util.List;
import java.util.Optional;

import com.bee.platform.common.interceptor.AuthorInterceptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import com.bee.platform.common.config.property.AuthConfigProperties;
import com.google.common.base.Splitter;
import com.google.common.collect.Lists;


/**
 * @author Raphael.dq
 * @date 2019/05/21
 */
@Configuration
@ConditionalOnProperty(prefix = "bee.common.auth", name = "switch-on", havingValue = "true")
public class AuthConfig implements WebMvcConfigurer {
    
    @Autowired
    AuthConfigProperties properties;
    
    @Bean
    public AuthorInterceptor commonAuthInterceptor() {
        return new AuthorInterceptor();
    }
    
     @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(commonAuthInterceptor())
                .addPathPatterns("/**")
                .excludePathPatterns("/swagger-resources/**", "/webjars/**", "/v2/**", "/swagger-ui.html/**")
                .excludePathPatterns(getExcludePaths());
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/**")
                .addResourceLocations("classpath:/static/");
        registry.addResourceHandler("/swagger-ui.html")
                .addResourceLocations("classpath:/META-INF/resources/");
        registry.addResourceHandler("/webjars/**")
                .addResourceLocations("classpath:/META-INF/resources/webjars/");
    }
    
    private List<String> getExcludePaths() {
        return Optional.ofNullable(properties.getExcludes())
            .map(s -> Splitter.on(",").trimResults().omitEmptyStrings().splitToList(s))
            .orElse(Lists.newArrayList());
    }
    
    
    
}
