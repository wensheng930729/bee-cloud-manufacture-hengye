package com.bee.platform.cloud.si.manufacture.config;

import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.client.transport.TransportClient;
import org.elasticsearch.common.settings.Settings;
import org.elasticsearch.common.transport.InetSocketTransportAddress;
import org.elasticsearch.transport.client.PreBuiltTransportClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.annotation.PostConstruct;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.List;

@Slf4j
@Configuration
public class ElasticSearchConfig {
	
	@Autowired
	private EsConfigurationProperties esConfigurationProperties;
	
    @PostConstruct
    void init() {
        System.setProperty("es.set.netty.runtime.available.processors", "false");
    }
    
    @Bean
    public TransportClient transportClient() {
    	// 设置es
    	Settings settings = Settings.builder()
    	        .put("cluster.name", esConfigurationProperties.getClusterName())
    	        .put("client.transport.sniff", true)
    	        .build();
    	TransportClient transportClient = new PreBuiltTransportClient(settings);
    	List<String> clusterNodes = esConfigurationProperties.getClusterNodes();
    	if(null == clusterNodes || clusterNodes.size() < 1) {
    		log.error("Elasticsearch config is empty!");
    		throw new RuntimeException("Elasticsearch config is empty!");
    	}
    	// 遍历配置
		clusterNodes.stream().forEach((x)-> {
			try {
				String[] esAddrArray = x.split(":");
				if (null != esAddrArray && esAddrArray.length > 0) {
					String hostName = esAddrArray[0];
					String port = esAddrArray[1];
					transportClient.addTransportAddress(new InetSocketTransportAddress(InetAddress.getByName(hostName), Integer.parseInt(port)));
				}
			} catch (UnknownHostException e) {
				log.error("Elasticsearch config is error!",e);
				throw new RuntimeException("Elasticsearch config is error!");
			}
		});
    	return transportClient;
    } 
}
