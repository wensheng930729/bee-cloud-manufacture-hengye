package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: RevisionStorageRecordDTO
 * @Description: 盘库记录实体类
 * @Author: fei.sun
 * @Date: 2019/9/29 9:24
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("盘库记录实体类")
public class RevisionStorageRecordDTO {

    @ApiModelProperty("货物名称")
    private String productName;

    @ApiModelProperty("盘库后数量")
    private BigDecimal reviseProductNumber;

    @ApiModelProperty("盘库前数量")
    private BigDecimal currentProductNumber;

    @ApiModelProperty("盘库数量")
    private BigDecimal reviseAmount;

    @ApiModelProperty("货物单位")
    private String productUnit;

    @ApiModelProperty("盘库人")
    private String creator;

    @ApiModelProperty("盘库时间")
    private String revisionTime;
}
