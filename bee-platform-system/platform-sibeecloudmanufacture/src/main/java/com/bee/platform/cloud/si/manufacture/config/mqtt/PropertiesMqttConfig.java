package com.bee.platform.cloud.si.manufacture.config.mqtt;

import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

import java.io.Serializable;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-10-10 13:30
 **/
@Data
@Component
@NoArgsConstructor
public class PropertiesMqttConfig implements Serializable {

    private static final long serialVersionUID = 6284879882875367739L;
    /**
     * mqtt 地址
     */
    @Value("${mqtt.host}")
    private String host;
    /**
     *  客户端id
     */
    @Value(value = "${mqtt.clientId}")
    private String clientId;
    /**
     * mqtt账号
     */
    @Value("${mqtt.username}")
    private String username;
    /**
     * mqtt 密码
     */
    @Value("${mqtt.password}")
    private String password;
    /**
     * mqtt 连接超时
     */
    @Value("${mqtt.keepAlive}")
    private String keepAlive;
    /**
     * 华辰智通获取数据订阅的主题
     */
    @Value("${mqtt.hignton.subsribeTopic}")
    private String subsribeTopic;
    /**
     * 给华辰智通发送消息的主题
     */
    @Value("${mqtt.hignton.pushTopic}")
    private String pushTopic;
    /**
     * 环境
     */
    @Value("${spring.profiles.active}")
    private String active;
}
