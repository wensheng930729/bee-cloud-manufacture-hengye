package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: ProductDTO
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/9/27 15:51
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("领用物下拉框实体类")
public class ProductDTO {

    @ApiModelProperty("领用物id")
    private Integer productId;

    @ApiModelProperty("领用物名称")
    private String productName;
}
