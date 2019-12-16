package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/9/26
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "捣炉记录请求参数")
public class ProStirFurnaceRecordRQ implements Serializable {
    private static final long serialVersionUID = -5401805051419361818L;

    @ApiModelProperty(value = "电极:1号电极，2号电极，3号电极")
    private Integer electrode;

    @ApiModelProperty(value = "捣炉时间段")
    private List<String> periodTime;
}
