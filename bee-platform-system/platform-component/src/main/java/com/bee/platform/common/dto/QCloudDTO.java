package com.bee.platform.common.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@ApiModel("腾讯云文件上传返回类型")
public class QCloudDTO implements Serializable{
	 
	private static final long serialVersionUID = -4924316910563816861L;
	
	@ApiModelProperty(value="随机的一个id")
	private String vid;
	
	@ApiModelProperty(value="返回的url")
	private String access_url;
	
	@ApiModelProperty(value="返回的url")
	private String resource_path;
	
	@ApiModelProperty(value="返回的url")
	private String source_url;
	
	@ApiModelProperty(value="返回的url")
	private String url;
	

}
