package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/26
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "电极消耗记录请求参数")
public class ProElectrodeRQ implements Serializable {
    private static final long serialVersionUID = 1718797395791085120L;

    @ApiModelProperty(value = "电极:1号电极，2号电极，3号电极")
    private Integer electrode;

    @ApiModelProperty(value = "压入时间次数")
    private List<String> periodTime;

    @ApiModelProperty(value = "接时长度")
    private String startLength;

    @ApiModelProperty(value = "交时长度")
    private String endLength;

    @ApiModelProperty(value = "接长")
    private String length;

    @ApiModelProperty(value = "电极总耗")
    private String consumption;

    @ApiModelProperty(value = "电极糊用量")
    private String electrodePaste;

    @ApiModelProperty(value = "断电极：1是，0否")
    private Integer disconnect;

    @ApiModelProperty(value = "处理机制")
    private String dealMechanism;
}
