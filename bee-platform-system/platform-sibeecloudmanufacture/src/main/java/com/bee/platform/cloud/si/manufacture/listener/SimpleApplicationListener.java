//package com.bee.platform.cloud.si.manufacture.listener;
//
//import com.baomidou.mybatisplus.mapper.EntityWrapper;
//import com.bee.platform.cloud.si.manufacture.config.mqtt.PropertiesMqttConfig;
//import com.bee.platform.cloud.si.manufacture.entity.PlcFactoryGateway;
//import com.bee.platform.cloud.si.manufacture.mqtt.ApolloServer;
//import com.bee.platform.cloud.si.manufacture.service.PlcFactoryGatewayService;
//import lombok.extern.slf4j.Slf4j;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.boot.context.event.ApplicationReadyEvent;
//import org.springframework.context.ApplicationListener;
//import org.springframework.context.ConfigurableApplicationContext;
//import org.springframework.stereotype.Component;
//import org.springframework.util.CollectionUtils;
//import org.springframework.web.method.HandlerMethod;
//import org.springframework.web.servlet.mvc.condition.PatternsRequestCondition;
//import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
//import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;
//
//import java.lang.reflect.Method;
//import java.util.Date;
//import java.util.List;
//import java.util.Map;
//
///**
// *  *  ApplicationListener<ApplicationReadyEvent>
// *  *  尽可能晚地发布事件，以指示应用程序已准备好服务请求。事件的源是SpringApplication本身，但是要注意修改其内部状态，因为所有初始化步骤都将在那时完成。
// * @description: 容器在所有的bean初始化完成后执行的方法
// * @author: junyang.li
// * @create: 2019-01-21 11:17
// **/
//@Slf4j
//@Component
//public class SimpleApplicationListener implements ApplicationListener<ApplicationReadyEvent> {
//
//    @Autowired
//    private ApolloServer apolloServer;
//
//    @Autowired
//    private PropertiesMqttConfig propertiesMqttConfig;
//
//    @Autowired
//    private PlcFactoryGatewayService plcFactoryGatewayService;
//    /**
//     * @notes: 容器初始化完成之后向华辰智通的网关发布一次时间同步帧
//     * @Author: junyang.li
//     * @Date: 13:24 2019/8/1
//     * @return: java.lang.Object
//     */
//    @Override
//    public void onApplicationEvent(ApplicationReadyEvent event) {
//        List<PlcFactoryGateway> factoryGateway=plcFactoryGatewayService.selectList(new EntityWrapper<>());
//        //判空
//        if(CollectionUtils.isEmpty(factoryGateway)){
//            return;
//        }
//        //向所有的网关发布时间同步帧
//       Date now=new Date();
//        factoryGateway.forEach(obj->{
//            String data="{ 'gid': '"+obj.getHcGatewayId()+"', 'ptid': 0, 'cid': 0," +
//                    " 'time':'"+now+"', 'func': 82,'server_datetime': '2017-09-04 08:46:40' }\\r\\n";
//            log.info("向所有的网关发送时间同步帧,数据是:{}",data);
//            apolloServer.send(propertiesMqttConfig.getPushTopic(),data);
//        });
//    }
//}
