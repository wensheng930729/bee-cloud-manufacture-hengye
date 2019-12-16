package com.bee.platform.cloud.si.manufacture;

import com.bee.platform.cloud.si.manufacture.socket.MeterDataServer;
import com.bee.platform.cloud.si.manufacture.socket.TCPServer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.stereotype.Component;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description Netty启动类
 * @Date ${date}
 */
@Slf4j
@Component
public class NettyBooter implements ApplicationListener<ContextRefreshedEvent> {


    @Override
    public void onApplicationEvent(ContextRefreshedEvent event) {
        try {
            //获取开始时间
            long startTimeTCPServer= System.currentTimeMillis();
            TCPServer.getInstance().startServer();
            //获取结束时间
            long endTimeTCPServer= System.currentTimeMillis();
            log.info("初始化TCPServer成功! 程序运行时间：{} ms",endTimeTCPServer-startTimeTCPServer);

            //获取开始时间
            long startTimeMeterDataServer=System.currentTimeMillis();
            MeterDataServer.getInstance().startServer();
            //获取结束时间
            long endTimeMeterDataServer=System.currentTimeMillis();
            log.info("初始化MeterDataServer成功! 程序运行时间：{} ms",endTimeMeterDataServer-startTimeMeterDataServer);
        } catch (Exception e) {
            e.getMessage();
        }
    }
}
