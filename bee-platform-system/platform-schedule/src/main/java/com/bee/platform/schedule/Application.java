package com.bee.platform.schedule;

import com.bee.platform.common.constants.FeignPackagesConstant;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.cloud.openfeign.EnableFeignClients;

/**
 * @description:
 * @author: zhigang.zhou
 * @create: 2019-03-01 11:21
 **/
@EnableFeignClients(basePackages= {FeignPackagesConstant.BUSINESS_FEIGN_BASEPACKAGE,
        FeignPackagesConstant.USER_FEIGN_BASEPACKAGE})
@SpringBootApplication(exclude = DataSourceAutoConfiguration.class)
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
