package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: SupplierOrCustomersDTO
 * @Date:
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("供应啥或客户下拉框实体类")
public class SupplierOrCustomersDTO {

    @ApiModelProperty("供应商或客户id")
    private Integer id;

    @ApiModelProperty("供应商或客户名称")
    private String name;
}
