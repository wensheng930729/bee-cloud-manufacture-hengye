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
@ApiModel(value = "配电记录返回信息")
public class ProPowerRecordDTO implements Serializable {

    private static final long serialVersionUID = -6212507216158340569L;

    @ApiModelProperty(value = "接时动力电")
    private String receive;

    @ApiModelProperty(value = "交时动力电")
    private String handover;

    @ApiModelProperty(value = "动力电消耗")
    private String consume;

    @ApiModelProperty(value = "配电记录明细返回信息")
    private List<ProPowerRecordDetailDTO> detailList;

}
