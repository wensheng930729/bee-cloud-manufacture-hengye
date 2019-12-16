package com.bee.platform.cloud.si.manufacture.mqtt;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.JSONObject;
import com.bee.platform.cloud.si.manufacture.config.mqtt.MqttClientFactory;
import com.bee.platform.cloud.si.manufacture.config.mqtt.PropertiesMqttConfig;
import com.bee.platform.cloud.si.manufacture.config.mqtt.QoS;
import com.bee.platform.cloud.si.manufacture.dao.mapper.MqttRealDataMapper;
import com.bee.platform.cloud.si.manufacture.dto.GatewayRealDataDTO;
import com.bee.platform.cloud.si.manufacture.dto.PlcRealDataDTO;
import com.bee.platform.cloud.si.manufacture.entity.MqttRealData;
import com.bee.platform.cloud.si.manufacture.service.PlcRealDataService;
import com.bee.platform.common.utils.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.*;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/**
 * @description: 接受mqtt中的消息
 * @author: junyang.li
 * @create: 2019-10-10 16:10
 **/
@Slf4j
@Component
public class ReceiveMqttMessage {

    @Autowired
    private MqttClientFactory mqttClientFactory;

    @Autowired
    private PropertiesMqttConfig propertiesMqttConfig;

    @Autowired
    private PlcRealDataService plcRealDataService;

    @Autowired
    private MqttRealDataMapper mqttRealDataMapper;

    /**
     * 队列
     */
    private BlockingQueue<String> queue=new ArrayBlockingQueue<>(5000);

    @PostConstruct
    private void receiveMessage(){
        //mqtt客户端对象
        MqttClient mqttClient=mqttClientFactory.getMqttClient();
        String topic=propertiesMqttConfig.getSubsribeTopic();
        //消息回调
        mqttClient.setCallback(new MqttMessageCallback(this.queue) {
            /**
             * 接受到消息
             */
            @Override
            public void messageArrived(String topic, MqttMessage message) {
                // subscribe后得到的消息会执行到这里面
                String value=new String(message.getPayload());
                try {
                    //TODO 暂时放置在这里，排查数据问题
                    //实时插入数据
                    mqttRealDataMapper.insert(new MqttRealData().setData(value).setTime(new Date()));
                    queue.put(value);
                }catch (Exception e){
                    log.error("将数据插入队列或数据库中异常，待插入数据是:{}。异常信息是:{}",value,e);
                }
            }

            /**
             * 断线重连成功后重新订阅主题
             */
            @Override
            public void connectComplete(boolean reconnect, String serverURI) {
                try {
                    mqttClient.subscribe(topic, QoS.QOS_1.getKey());
                    log.info("MQTT 断线重连成功，重新订阅主题{}成功。",topic);
                } catch (MqttException e) {
                    log.info("MQTT 断线重连主题{}失败，错误日志是:{}",topic,e);
                }
            }
        });
        try{
            //订阅主题
            mqttClient.subscribe(topic, QoS.QOS_2.getKey());
            log.info("MQTT 订阅主题{}成功",topic);
        }catch (MqttException e){
            log.info("MQTT 订阅主题：{}失败，等待重连...错误日志是:{}",topic,e);
        }
        //创建线程实例
        Task task=new Task(this.queue);
        task.start();
    }
    /**
     * @notes: 任务线程
     * @Author: junyang.li
     * @Date: 17:20 2019/10/10
     */
    private class Task extends Thread{

        private BlockingQueue<String> queue;

        private static final int REAL_DATA_FUNC=2;

        Task(BlockingQueue<String> queue){
            this.queue=queue;
        }

        @Override
        public void run() {
            while (true){
                try {
                    String value =this.queue.take();
                    //解析数据
                    GatewayRealDataDTO dto=this.analysisData(value);
                    if(dto!=null){
                        //处理实时数据
                        plcRealDataService.tacklePlcData(dto);
                    }
                } catch (Exception e) {
                    log.info("处理MQTT中的数据异常，异常信息是:{}",e);
                }
            }
        }
        /**
         * @notes: 解析队列中获取到的数据
         * @Author: junyang.li
         * @Date: 11:09 2019/10/11
         * @param str : 数据源
         * @return: void
         */
        private GatewayRealDataDTO analysisData(String str){
            JSONObject json;
            try {
                json= JSON.parseObject(str);
            }catch (JSONException e){
                log.info("接收消息内容 : {}",str);
                log.error("从MQTT中数据解析json失败，错误信息是:{}",e);
                return null;
            }
            //json数据判空 或者数据校验不通过
            boolean check=json==null || !this.checkData(json);
            if(check){
                return null;
            }
            //华辰智通返回的数据功能码
            int func=json.getInteger("func");
            //采集的不是实时数据，直接跳过
            if(REAL_DATA_FUNC != func){
                return null;
            }
            //获取实时数据
            JSONObject data=json.getJSONObject("points");
            if(data!=null){
                Map<String,String> map=new HashMap<>(16);
                data.getInnerMap().forEach((k,v)->map.put(k,v.toString()));
                //华辰智通的网关唯一编号
                String hcGatewayId=json.getString("gid");
                String time=json.getString("time");
                Date receiveTime = DateUtils.parse(time,null);
                if(receiveTime==null){
                    receiveTime=new Date();
                }
                return new GatewayRealDataDTO(hcGatewayId,map,receiveTime);
            }
            return null;
        }

        /**
         * @notes: 校验返回数据是否正确
         * @Author: junyang.li
         * @Date: 16:48 2019/10/10
         * @param json : 返回数据
         * @return: boolean
         */
        private boolean checkData(JSONObject json){
            //华辰智通返回的数据状态码
            Integer errCode=json.getInteger("err");
            if(errCode==null){
                return false;
            }
            if( HigntonErrCode.NORMAL.getCode().equals(errCode)){
                return true;
            }else if(HigntonErrCode.BREAK_CONNECT.getCode().equals(errCode)){
                log.error("mqtt获取数据失败,失败提示是:{}。返回的数据是:{}",
                        HigntonErrCode.BREAK_CONNECT.getDesc(),json.toString());
                return false;
            }else if(HigntonErrCode.READ_ERROR.getCode().equals(errCode)){
                log.error("mqtt获取数据失败,失败提示是:{}。返回的数据是:{}",
                        HigntonErrCode.READ_ERROR.getDesc(),json.toString());
            }
            return false;
        }
    }
}
