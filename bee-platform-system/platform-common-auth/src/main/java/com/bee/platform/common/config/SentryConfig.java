package com.bee.platform.common.config;

import io.sentry.Sentry;
import io.sentry.SentryClient;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

/**
 * @description: Sentry 日志采集
 * @author: junyang.li
 * @create: 2019-11-11 14:25
 **/
@Configuration
public class SentryConfig {

    public static SentryClient sentryClient;

    private static final String SENTRY_KEY="https://2c23feba0b5c4d32b57ba178d741e0c4@sentry.io/1815080";

    @Value("${spring.profiles.active}")
    private String active;
    @Value("${server.servlet.context-path}")
    private String appName;

    @Bean
    public SentryClient sentryClient() {
        SentryClient sentryClient = Sentry.init(SENTRY_KEY);
        sentryClient.setEnvironment(active);
        sentryClient.addTag("service", appName);
        SentryConfig.sentryClient=sentryClient;
        return sentryClient;
    }

    @PreDestroy
    public void destroy() {
        if(sentryClient!=null){
            sentryClient.closeConnection();
        }
    }
}
