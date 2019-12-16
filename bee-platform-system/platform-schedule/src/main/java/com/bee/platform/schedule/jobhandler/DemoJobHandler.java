package com.bee.platform.schedule.jobhandler;

import com.bee.platform.cloud.user.service.feign.UserCloudManufactureBeatFeignClient;
import com.bee.platform.common.entity.ResponseResult;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.IJobHandler;
import com.xxl.job.core.handler.annotation.JobHandler;
import com.xxl.job.core.log.XxlJobLogger;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

/**
 * @ClassName DemoJobHandler
 * @Description 任务Handler示例（Bean模式）
 * 开发步骤：
 * 1、继承"IJobHandler"：“com.xxl.job.core.handler.IJobHandler”；
 * 2、注册到Spring容器：添加“@Component”注解，被Spring容器扫描为Bean实例；
 * 3、注册到执行器工厂：添加“@JobHandler(value="自定义jobhandler名称")”注解，注解value值对应的是调度中心新建任务的JobHandler属性的值。
 * 4、执行日志：需要通过 "XxlJobLogger.log" 打印执行日志；
 * @author zhigang.zhou
 * @Date 2018年11月30日 下午1:16:35
 * @version 1.0.0
 */
@Slf4j
@JobHandler(value="demoJobHandler")
@Component
public class DemoJobHandler extends IJobHandler {
    
	@Autowired
	private UserCloudManufactureBeatFeignClient userCloudManufactureBeatFeignClient;

	@Override
	public ReturnT<String> execute(String param) throws Exception {
		XxlJobLogger.log("XXL-JOB, Hello World.");
		// 测试Feign客户端的调用
		ResponseResult<String> userBeatResult = userCloudManufactureBeatFeignClient.beat();
		log.info("userBeatResult=" + userBeatResult);
		for (int i = 0; i < 5; i++) {
			XxlJobLogger.log("beat at:" + i);
			TimeUnit.SECONDS.sleep(2);
		}
		return SUCCESS;
	}

}
