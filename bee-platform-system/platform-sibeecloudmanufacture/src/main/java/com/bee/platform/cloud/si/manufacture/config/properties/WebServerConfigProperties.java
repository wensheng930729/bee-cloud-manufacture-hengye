package com.bee.platform.cloud.si.manufacture.config.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import lombok.Getter;
import lombok.Setter;

@Configuration
@ConfigurationProperties(prefix = "server.tomcat")
@Getter
@Setter
public class WebServerConfigProperties {

	private Integer keepAliveTimeout = 30000;
	
	private Integer maxKeepAliveRequests = 10000;
	
}
