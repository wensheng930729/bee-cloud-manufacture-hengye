package com.bee.platform.cloud.si.manufacture.config;


import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.List;

@Configuration
@ConfigurationProperties(prefix = "spring.data.elasticsearch")
public class EsConfigurationProperties {
	
	private String clusterName;
	
	private List<String> clusterNodes;

	public String getClusterName() {
		return clusterName;
	}

	public void setClusterName(String clusterName) {
		this.clusterName = clusterName;
	}

	public List<String> getClusterNodes() {
		return clusterNodes;
	}

	public void setClusterNodes(List<String> clusterNodes) {
		this.clusterNodes = clusterNodes;
	}

}
