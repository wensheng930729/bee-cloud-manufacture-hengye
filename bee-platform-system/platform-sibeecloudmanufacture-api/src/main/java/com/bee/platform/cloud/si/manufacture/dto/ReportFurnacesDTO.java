package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @author ReportFurnacesDTO
 * @version 1.0.0
 * @Description 炉号返回
 * @Date 2019/10/23 14:06
 */
@Data
@Accessors(chain = true)
@ApiModel("炉号返回")
public class ReportFurnacesDTO {

        @ApiModelProperty("id")
        private Integer furnacesId;

        @ApiModelProperty("名称")
        private String furnacesName;

}
