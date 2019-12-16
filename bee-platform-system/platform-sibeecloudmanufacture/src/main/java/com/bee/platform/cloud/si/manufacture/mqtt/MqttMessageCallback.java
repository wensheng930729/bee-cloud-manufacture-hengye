package com.bee.platform.cloud.si.manufacture.mqtt;

import lombok.extern.slf4j.Slf4j;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallbackExtended;
import org.eclipse.paho.client.mqttv3.MqttMessage;

import java.util.concurrent.BlockingQueue;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-09-29 11:24
 **/
@Slf4j
public abstract class MqttMessageCallback implements MqttCallbackExtended {
    /**
     * 队列
     */
    private BlockingQueue<String> queue;


    public MqttMessageCallback(BlockingQueue<String> queue){

        this.queue=queue;
    }

    /**
     * 连接断开，可以做重连
     */
    @Override
    public  void connectionLost(Throwable cause){
        log.error("MQTT 断线自动重连中....");
    }

    /**
     * 发送完成后回调
     */
    @Override
    public void deliveryComplete(IMqttDeliveryToken token) {
        log.info("deliveryComplete---------{}" , token.isComplete());
    }

    /**
     * 接收到数据回调
     */
    @Override
    public abstract void messageArrived(String topic, MqttMessage message);

    /**
     *  连接成功
     */
    @Override
    public abstract void connectComplete(boolean reconnect, String serverURI);
}
