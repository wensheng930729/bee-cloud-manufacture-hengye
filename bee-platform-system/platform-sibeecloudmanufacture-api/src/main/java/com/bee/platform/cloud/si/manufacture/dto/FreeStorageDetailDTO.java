package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: FreeStorageDetailDTO
 * @Description: 自由入库实体类
 * @Author: fei.sun
 * @Date: 2019/9/24 16:54
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("自由入库实体类")
public class FreeStorageDetailDTO {

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品数量")
    private BigDecimal productNumber;

    @ApiModelProperty("仓库编号")
    private Integer storageId;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;
}
