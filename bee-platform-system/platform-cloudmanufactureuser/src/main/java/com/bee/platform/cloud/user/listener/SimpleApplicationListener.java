package com.bee.platform.cloud.user.listener;


import com.bee.platform.cloud.user.service.resource.CancelResourceStrategyService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 *  *  ApplicationListener<ApplicationReadyEvent>
 *  *  尽可能晚地发布事件，以指示应用程序已准备好服务请求。事件的源是SpringApplication本身，但是要注意修改其内部状态，因为所有初始化步骤都将在那时完成。
 * @description: 容器在所有的bean初始化完成后执行的方法
 * @author: junyang.li
 * @create: 2019-01-21 11:17
 **/
@Slf4j
@Component
public class SimpleApplicationListener implements ApplicationListener<ApplicationReadyEvent> {

    @Autowired
    private CancelResourceStrategyService strategyService;
    /**
     * @notes: 容器初始化完成之后执行资源相关的策略分发初始化方法
     * @Author: junyang.li
     * @Date: 13:24 2019/8/1
     * @return: java.lang.Object
     */
    @Override
    public void onApplicationEvent(ApplicationReadyEvent event) {
        strategyService.init();
    }
}
