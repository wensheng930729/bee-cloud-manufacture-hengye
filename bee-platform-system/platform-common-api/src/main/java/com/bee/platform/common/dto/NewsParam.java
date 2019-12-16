package com.bee.platform.common.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

@ApiModel("资讯删除传入参数")
public class NewsParam implements Serializable{


	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("企业ID")
    @NotNull(message = "企业ID不能为空")
    private List<Integer> newIds;

	public List<Integer> getNewIds() {
		return newIds;
	}

	public void setNewIds(List<Integer> newIds) {
		this.newIds = newIds;
	}
	
	
}
