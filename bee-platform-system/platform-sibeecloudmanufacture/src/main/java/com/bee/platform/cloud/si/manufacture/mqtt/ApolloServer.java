package com.bee.platform.cloud.si.manufacture.mqtt;

import com.bee.platform.cloud.si.manufacture.config.mqtt.MqttClientFactory;
import com.bee.platform.cloud.si.manufacture.config.mqtt.QoS;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.paho.client.mqttv3.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


@Slf4j
@Component
public class ApolloServer {

    @Autowired
    private MqttClientFactory mqttClientFactory;

    private MqttTopic topic;

    private MqttMessage message;

    public boolean send(String topicStr, String content) {
        MqttClient mqttClient=mqttClientFactory.getMqttClient();
        try {
            // 若中途有网络闪断,则重连
            if (!mqttClient.isConnected()) {
                mqttClient.reconnect();
            }
            topic = mqttClient.getTopic(topicStr);
            message = new MqttMessage();
            message.setPayload(content.getBytes());
            message.setQos(QoS.QOS_2.getKey());
            message.setRetained(false);
            MqttDeliveryToken token = topic.publish(message);
            token.waitForCompletion();
            log.info("send data to mqtt successfully, topic: {}, data: {}", topicStr, content);
            return true;
        } catch (MqttException e) {
            log.error("send data to mqtt error, topic: {}, data: {}", topicStr, content, e);
            return false;
        }
    }
}
