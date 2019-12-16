package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName: BulkOutTonBagListDTO
 * @Description: 批量出库吨袋实体类
 * @Author: fei.sun
 * @Date: 2019/9/27 13:37
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("批量出库吨袋实体类")
public class BulkOutTonBagListDTO {

    @ApiModelProperty("待出库车辆唯一标识id")
    private String contractCarId;

    @ApiModelProperty("吨袋信息集合")
    private List<TonBagDTO> tonBagNumbers;
}
