package com.bee.platform.cloud.si.manufacture.config.mqtt;


import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.Random;

/**
 * @notes: mqtt client factory
 * @Author: junyang.li
 * @Date: 14:26 2019/10/10
 */
@Slf4j
@Component
public class MqttClientFactory {

	@Autowired
	private PropertiesMqttConfig propertiesMqttConfig;
	/**
	 * mqtt 客户端
	 */
	private MqttClient mqttClient;

	/**
	 * 开发环境随机生成客户端，避免相互挤下线
	 */
	private static final String ACTIVE="prod";
	/**
	 * @notes:  获取mqtt客户端
	 * @Author: junyang.li
	 * @Date: 14:31 2019/10/10
	 * @return: org.eclipse.paho.client.mqttv3.MqttClient
	 */
	public synchronized MqttClient  getMqttClient(){
		if(this.mqttClient==null){
			this.mqttClient =this.mqttClient();
		}
		return this.mqttClient;
	}
    /**
     * @notes:  根据参数创建mqtt客户端
     * @Author: junyang.li
     * @Date: 14:32 2019/10/10
     * @return: org.eclipse.paho.client.mqttv3.MqttClient
     */
	private MqttClient mqttClient() {
		try {
			MqttClient mqttClient = new MqttClient(propertiesMqttConfig.getHost(), this.getClientId(),
					new MemoryPersistence());
			MqttConnectOptions options = new MqttConnectOptions();
			// 设置是否清空session,这里如果设置为false表示服务器会保留客户端的连接记录，
			// 这里设置为false，客户端离线保存会话消息，避免上线发布过程中消息丢失
			options.setCleanSession(true);
			// 设置连接的用户名
			options.setUserName(propertiesMqttConfig.getUsername());
			// 设置连接的密码
			options.setPassword(propertiesMqttConfig.getPassword().toCharArray());
			short sh=Short.valueOf(propertiesMqttConfig.getKeepAlive());
			// 设置超时时间 单位为秒
			options.setConnectionTimeout(sh);
			// 设置会话心跳时间 单位为秒 服务器会每隔1.5*20秒的时间向客户端发送个消息判断客户端是否在线，但这个方法并没有重连的机制
			options.setKeepAliveInterval(sh);
			//设置自动断线重连
			options.setAutomaticReconnect(true);
			mqttClient.connect(options);
			return mqttClient;
		} catch (MqttException e) {
			throw new RuntimeException("创建MqttClient失败！");
		}
	}
	/**
	 * @notes: 判断是否是开发环境，如果是开发环境，则随机生成一个mqtt clientId 避免
     * 大家运行项目MQTT被挤下线后重复断线重连
 	 * @Author: junyang.li
	 * @Date: 12:15 2019/10/11
	 * @return: java.lang.String
	 */
	private String getClientId(){
		if(!ACTIVE.equals(propertiesMqttConfig.getActive())){
			return propertiesMqttConfig.getClientId()+
					DateUtils.format(new Date(),DateUtils.YMDHMS);
		}
		return propertiesMqttConfig.getClientId();

    }
}