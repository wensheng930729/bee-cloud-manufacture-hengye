package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @ClassName: FinishedProductInStorageDetailDTO
 * @Description: 产成品入库详情实体类
 * @Author: fei.sun
 * @Date: 2019/10/6 15:42
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("产成品入库详情实体类")
public class FinishedProductInStorageDetailDTO {

    @ApiModelProperty("入库时间")
    private String inStorageTime;

    @ApiModelProperty("入库数量")
    private BigDecimal productNumber;

    @ApiModelProperty("吨袋信息")
    private List<InStorageTonBagDTO> inStorageTonBagDTOS;
}
