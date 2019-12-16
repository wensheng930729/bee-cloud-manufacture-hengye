package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: PickOutStorageDTO
 * @Description: 领用出库记录实体类
 * @Author: fei.sun
 * @Date: 2019/9/28 11:43
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("领用出库记录实体类")
public class PickOutStorageDTO {

    @ApiModelProperty("领用人")
    private String receiver;

    @ApiModelProperty("领用物")
    private String productName;

    @ApiModelProperty("仓库")
    private String storageName;

    @ApiModelProperty("领用数量")
    private BigDecimal productNumber;

    @ApiModelProperty("出库时间")
    private String outStorageTime;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

}
