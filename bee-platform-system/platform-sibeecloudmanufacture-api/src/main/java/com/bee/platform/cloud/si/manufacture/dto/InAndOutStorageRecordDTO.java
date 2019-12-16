package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: InAndOutStorageRecordDTO
 * @Description: 库存管理出入库记录实体类
 * @Author: fei.sun
 * @Date: 2019/9/29 11:43
 * @Version: 1.0
 */

@Accessors(chain = true)
@Data
@ApiModel("库存管理出入库记录实体类")
public class InAndOutStorageRecordDTO {

    @ApiModelProperty("出/入库时间")
    private String time;

    @ApiModelProperty("出/入库数量")
    private BigDecimal productNumber;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("出库人")
    private String creator;

}
