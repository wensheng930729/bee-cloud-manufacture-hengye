package com.bee.platform.cloud.si.manufacture.config;

import com.alibaba.druid.support.http.StatViewServlet;
import com.alibaba.druid.support.http.WebStatFilter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @ClassName DruidConfiguration
 * @Description druid 配置. 这样的方式不需要添加注解：@ServletComponentScan 使用：http://127.0.0.1:8810/platform-sibeecloudmanufacture/druid/index.html
   *                 或者http://127.0.0.1:8810/platform-sibeecloudmanufacture/druid这个地址就能访问数据库的监控了
 * @author zhigang.zhou
 * @Date 2019年09月16日 下午13:16:35
 * @version 1.0.0
 */
@Configuration
public class  DruidConfiguration{
    
    @Value("${druid.monitor.loginUsername}")
    private String loginUsername;
    
    @Value("${druid.monitor.loginPassword}")
    private String loginPassword;
    
    @Bean
    public ServletRegistrationBean<StatViewServlet> druidStatViewServle2() {
        // org.springframework.boot.context.embedded.ServletRegistrationBean提供类的进行注册.
        ServletRegistrationBean<StatViewServlet> servletRegistrationBean = new ServletRegistrationBean<>(
                new StatViewServlet(), "/druid/*");
        // 添加初始化参数：initParams
        // 白名单：
        servletRegistrationBean.addInitParameter("allow", "127.0.0.1");
        // IP黑名单 (存在共同时，deny优先于allow) : 如果满足deny的话提示:Sorry, you are not permitted to view this page.
        // servletRegistrationBean.addInitParameter("deny", "192.168.1.73");
        // 登录查看信息的账号密码.
        servletRegistrationBean.addInitParameter("loginUsername", loginUsername);
        servletRegistrationBean.addInitParameter("loginPassword", loginPassword);
        // 是否能够重置数据.
        servletRegistrationBean.addInitParameter("resetEnable", "false");
        return servletRegistrationBean;
    }

    /**
     * @Description 注册一个：filterRegistrationBean
     */
    @Bean
    public FilterRegistrationBean<WebStatFilter> druidStatFilter2() {
        FilterRegistrationBean<WebStatFilter> filterRegistrationBean = new FilterRegistrationBean<>(
                new WebStatFilter());
        // 添加过滤规则.
        filterRegistrationBean.addUrlPatterns("/*");
        // 添加不需要忽略的格式信息.
        filterRegistrationBean.addInitParameter("exclusions", "*.js,*.gif,*.jpg,*.png,*.css,*.ico,/druid2/*");
        return filterRegistrationBean;
    }

}
