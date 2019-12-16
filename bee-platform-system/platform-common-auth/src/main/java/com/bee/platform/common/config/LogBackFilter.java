package com.bee.platform.common.config;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.filter.ThresholdFilter;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;
import io.sentry.SentryClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @description:  logback 日志过滤，只过滤ERROR级别日志
 * @author: junyang.li
 * @create: 2019-11-11 15:54
 **/

public class LogBackFilter extends ThresholdFilter {

    @Override
    public FilterReply decide(ILoggingEvent event) {
        /*if(Level.ERROR.levelInt==event.getLevel().levelInt){
            if(SentryConfig.sentryClient!=null){
                //加入sentry日志采集
                SentryConfig.sentryClient.sendMessage(event.getFormattedMessage());
            }
        }*/
        return super.decide(event);
    }
}
