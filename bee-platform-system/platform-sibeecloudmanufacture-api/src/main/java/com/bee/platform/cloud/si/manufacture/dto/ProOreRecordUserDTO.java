package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author xin.huang
 * @description
 * @date 2019/10/31
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "矿热炉记录人员返回基本信息")
public class ProOreRecordUserDTO implements Serializable {
    private static final long serialVersionUID = -9048576212012986888L;

    @ApiModelProperty(value = "记录状态：0没有记录员，1有其他记录员，2存在记录员，且是当前用户")
    private Integer onduty;

    @ApiModelProperty(value = "矿热炉记录id")
    private Long oreRecordId;

    @ApiModelProperty(value = "记录员姓名")
    private String name;

    @ApiModelProperty(value = "记录员电话")
    private String phone;
}
