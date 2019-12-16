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
@ApiModel(value = "出炉记录请求参数")
public class ProOutFurnaceRecordRQ implements Serializable {
    private static final long serialVersionUID = 1822515347514268511L;

    @ApiModelProperty(value = "出炉时间段")
    private List<String> periodTime;
}
