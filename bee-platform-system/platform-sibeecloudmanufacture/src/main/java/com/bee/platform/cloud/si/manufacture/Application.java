package com.bee.platform.cloud.si.manufacture;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.circuitbreaker.EnableCircuitBreaker;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableScheduling;
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
        FeignPackagesConstant.USERMANUFACTURE_FEIGN_BASEPACKAGE,
        FeignPackagesConstant.SIMANUFACTURE_FEIGN_BASEPACKAGE
        })
@ComponentScan({BaseScanPackagesConstant.COMMON_BASEPACKAGE,
        BaseScanPackagesConstant.SIMANUFACTURE_BASEPACKAGE,
        BaseScanPackagesConstant.USERMANUFACTURE_BASEPACKAGE
})
@MapperScan(basePackages = {MapperScanPackagesConstant.USER_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.COMMON_MAPPER_BASEPACKAGE,
        MapperScanPackagesConstant.SIMANUFACTURE_MAPPER_BASEPACKAGE
})
@EnableScheduling
public class Application {

    public static void main(String[] args) {
        try {
            SpringApplication.run(Application.class, args);
        } catch (Exception e) {
            System.out.println(e);
        }

    }
}
