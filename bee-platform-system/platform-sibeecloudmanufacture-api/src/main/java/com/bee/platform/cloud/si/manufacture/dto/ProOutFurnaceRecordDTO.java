package com.bee.platform.cloud.si.manufacture.dto;

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
@ApiModel(value = "出炉记录返回信息")
public class ProOutFurnaceRecordDTO implements Serializable {

    private static final long serialVersionUID = -1848336428151364039L;

    @ApiModelProperty(value = "出炉时间段")
    private List<String> periodTime;
}
