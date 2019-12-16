package com.bee.platform.cloud.si.manufacture.config;

import com.bee.platform.common.utils.ConstantsUtil;
import feign.RequestInterceptor;
import feign.RequestTemplate;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class FeignConfig {

	@Bean
    public RequestInterceptor requestInterceptor() {

        return new RequestInterceptor() {

            @Override
            public void apply(RequestTemplate template) {
                template.header(ConstantsUtil.INNER_CLIENT_ID,ConstantsUtil.INNER_SIMANUFACTURE);
            }

        };
    }
	
}
