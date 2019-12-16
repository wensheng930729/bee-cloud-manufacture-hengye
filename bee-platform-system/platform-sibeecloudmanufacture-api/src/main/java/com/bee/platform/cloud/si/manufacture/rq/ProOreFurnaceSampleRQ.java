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
 * @date 2019/10/8
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "出炉取样推送请求参数")
public class ProOreFurnaceSampleRQ implements Serializable {
    private static final long serialVersionUID = -6295801869075287844L;

    @ApiModelProperty(value = "矿热炉记录id")
    private Long oreRecordId;

    @ApiModelProperty(value = "炉号id")
    private Integer furnaceId;

    @ApiModelProperty(value = "班次:1一班，2二班，3三班")
    private Integer shift;

    @ApiModelProperty(value = "出炉批次:1一次，2二次，3三次")
    private Integer furnaceBatch;

    @ApiModelProperty(value = "是否取样 0 未取样 1已取样")
    private Integer sampleStatus;

    @ApiModelProperty(value = "开班时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date openTime;

}
