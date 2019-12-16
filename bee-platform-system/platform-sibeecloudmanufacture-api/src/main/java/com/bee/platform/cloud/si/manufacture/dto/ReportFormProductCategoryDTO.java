package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: ReportFormProductCategoryDTO
 * @Description:
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("报表产品类别返回数据")
public class ReportFormProductCategoryDTO {

    @ApiModelProperty("产品类别id")
    private Integer categoryId;

    @ApiModelProperty("产品类别名称")
    private String categoryName;

}
