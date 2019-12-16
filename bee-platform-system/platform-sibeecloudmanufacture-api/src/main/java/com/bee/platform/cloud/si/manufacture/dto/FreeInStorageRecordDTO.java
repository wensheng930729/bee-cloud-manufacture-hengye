package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: FreeInStorageRecordDTO
 * @Description: 新增入库记录列表实体类
 * @Author: fei.sun
 * @Date: 2019/9/30 14:21
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("新增入库记录列表实体类")
public class FreeInStorageRecordDTO {

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("入库数量")
    private BigDecimal productNumber;

    @ApiModelProperty("入库时间")
    private String time;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;
}
