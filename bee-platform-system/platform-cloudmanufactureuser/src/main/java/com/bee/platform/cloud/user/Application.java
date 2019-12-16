package com.bee.platform.cloud.user;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.circuitbreaker.EnableCircuitBreaker;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.bee.platform.common.constants.BaseScanPackagesConstant;
import com.bee.platform.common.constants.FeignPackagesConstant;
import com.bee.platform.common.constants.MapperScanPackagesConstant;

/**
 * @ClassName Application
 * @Description  蜜云制造-硅系
 * @author zhigang.zhou
 * @Date 2019年09月16日 下午13:16:35
 * @version 1.0.0
 */
@SpringBootApplication
@EnableCircuitBreaker//对hystrixR熔断机制的支持
@EnableAutoConfiguration
@EnableTransactionManagement
@EnableFeignClients(basePackages= {
		FeignPackagesConstant.USERMANUFACTURE_FEIGN_BASEPACKAGE
        })
@ComponentScan({BaseScanPackagesConstant.COMMON_BASEPACKAGE,
        BaseScanPackagesConstant.USERMANUFACTURE_BASEPACKAGE
})
@MapperScan(basePackages = {MapperScanPackagesConstant.USERMANUFACTURE_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.COMMON_MAPPER_BASEPACKAGE
})
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
