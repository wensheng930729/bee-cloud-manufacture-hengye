package com.bee.platform.cloud.si.manufacture.service.feign;

import com.bee.platform.cloud.si.manufacture.config.SiBeeCloudManufactureBeatFeignLoggerConfig;
import com.bee.platform.cloud.si.manufacture.hystrix.SiBeeCloudManufactureBeatFeignClientFallbackFactory;
import com.bee.platform.common.entity.ResponseResult;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;


@FeignClient(name="platform-sibeecloudmanufacture-beat",url="${platform-sibeecloudmanufacture.remote-addr}", configuration= {SiBeeCloudManufactureBeatFeignLoggerConfig.class}, fallbackFactory= SiBeeCloudManufactureBeatFeignClientFallbackFactory.class)
public interface SiBeeCloudManufactureBeatFeignClient {

    @RequestMapping(method = RequestMethod.GET, value = "/platform-sibeecloudmanufacture/beat")
    ResponseResult<String> beat();
    
}
