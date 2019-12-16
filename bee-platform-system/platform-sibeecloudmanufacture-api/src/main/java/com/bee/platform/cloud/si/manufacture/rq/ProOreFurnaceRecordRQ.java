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
@ApiModel(value = "矿热炉记录新增请求信息")
public class ProOreFurnaceRecordRQ implements Serializable {
    private static final long serialVersionUID = -863500797026437119L;

    @ApiModelProperty(value = "id")
    private Long id;

    @ApiModelProperty(value = "炉号id")
    private Integer furnaceId;

    @ApiModelProperty(value = "炉号名称")
    private String furnaceName;

    @ApiModelProperty(value = "班次:1一班，2二班，3三班")
    private Integer shift;

    @ApiModelProperty(value = "当前是否有人值班：1是，0否")
    private Integer onduty;

    @ApiModelProperty(value = "开班时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date openTime;

}
