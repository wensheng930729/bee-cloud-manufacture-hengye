package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: InStorageTonBagDTO
 * @Description: 入库详情吨袋实体类
 * @Author: fei.sun
 * @Date: 2019/10/6 15:49
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("入库详情吨袋实体类")
public class InStorageTonBagDTO {

    @ApiModelProperty("吨袋编号")
    private String tonBagNumber;

    @ApiModelProperty("吨袋重量")
    private BigDecimal productNumber;

    @ApiModelProperty("吨袋入库时间")
    private String inStorageTime;
}
