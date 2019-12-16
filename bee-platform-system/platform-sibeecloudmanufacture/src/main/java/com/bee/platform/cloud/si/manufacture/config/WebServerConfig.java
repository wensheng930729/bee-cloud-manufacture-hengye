package com.bee.platform.cloud.si.manufacture.config;

import org.apache.catalina.connector.Connector;
import org.apache.coyote.http11.Http11NioProtocol;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.web.embedded.tomcat.TomcatConnectorCustomizer;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.boot.web.servlet.server.ConfigurableServletWebServerFactory;
import org.springframework.stereotype.Component;

import com.bee.platform.cloud.si.manufacture.config.properties.WebServerConfigProperties;

import lombok.extern.slf4j.Slf4j;

/**
 * @ClassName WebServerConfig
 * @Description springboot内嵌Tomcat的长连接参数的优化: 当Spring容器内没有TomcatEmbeddedServletContainerFactory这个bean时，会把此bean加载进spring容器中。
 * @author zhigang.zhou
 * @Date 2019年09月16日 下午13:16:35
 * @version 1.0.0
 */
@Slf4j
@Component
public class WebServerConfig implements WebServerFactoryCustomizer<ConfigurableServletWebServerFactory> {

	@Autowired
	private WebServerConfigProperties webServerConfigProperties;

	@Override
	public void customize(ConfigurableServletWebServerFactory configurableServletWebServerFactory) {
		// 使用对应工厂类提供给我们的接口定制化我们的tomcat connector
		if (configurableServletWebServerFactory instanceof TomcatServletWebServerFactory) {
			TomcatServletWebServerFactory tomcatServletWebServerFactory = (TomcatServletWebServerFactory) configurableServletWebServerFactory;
			tomcatServletWebServerFactory.addConnectorCustomizers(new TomcatConnectorCustomizer() {
				@Override
				public void customize(Connector connector) {
					try {
						Http11NioProtocol protocol = (Http11NioProtocol) connector.getProtocolHandler();
						// 定制化keepalivetimeout,设置指定的秒数内没有请求则服务端自动断开keepalive链接
						protocol.setKeepAliveTimeout(webServerConfigProperties.getKeepAliveTimeout());
						// 当客户端发送超过所设置的请求个数的时候则自动断开keepalive链接
						protocol.setMaxKeepAliveRequests(webServerConfigProperties.getMaxKeepAliveRequests());
						log.info("start optimized server config args success");
					} catch (Exception e) {
						log.error("start optimized server config args error");
					}
				}
			});
		}
	}
}
