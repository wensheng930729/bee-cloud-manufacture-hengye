package com.bee.platform.cloud.user.service.feign;

import com.bee.platform.cloud.user.config.UserCloudManufactureBeatFeignLoggerConfig;
import com.bee.platform.cloud.user.hystrix.UserCloudManufactureBeatFeignClientFallbackFactory;
import com.bee.platform.common.entity.ResponseResult;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@FeignClient(name="platform-cloudmanufactureuser-beat",url="${platform-cloudmanufactureuser.remote-addr}", configuration= {UserCloudManufactureBeatFeignLoggerConfig.class}, fallbackFactory= UserCloudManufactureBeatFeignClientFallbackFactory.class)
public interface UserCloudManufactureBeatFeignClient {

    @RequestMapping(method = RequestMethod.GET, value = "/platform-cloudmanufactureuser/beat")
    ResponseResult<String> beat();
    
}
