package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/26
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "矿热炉记录查询请求信息")
public class ProOreFurnaceRecordQueryRQ implements Serializable {
    private static final long serialVersionUID = -2514006753691445787L;

    @ApiModelProperty(value = "炉号id")
    private Integer furnaceId;

    @ApiModelProperty(value = "班次:1一班，2二班，3三班")
    private Integer shift;

    @ApiModelProperty(value = "企业id")
    private Integer companyId;

    @ApiModelProperty(value = "工厂id")
    private Integer factoryId;

    @ApiModelProperty(value = "当前是否有人值班：1是，0否")
    private Integer onduty;

    @ApiModelProperty(value = "矿热炉记录id")
    private Long oreRecordId;

    @ApiModelProperty(value = "开班时间")
    private String openTime;

}
