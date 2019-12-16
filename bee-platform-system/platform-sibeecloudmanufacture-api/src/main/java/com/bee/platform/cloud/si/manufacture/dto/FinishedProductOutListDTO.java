package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName: FinishedProductOutListDTO
 * @Description: 成品出库列表实体类
 * @Author: fei.sun
 * @Date: 2019/11/15 16:07
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("成品出库列表实体类")
public class FinishedProductOutListDTO {

    @ApiModelProperty("按吨袋出库实体类")
    BulkOutTonBagListDTO bulkOutTonBagListDTO;

    @ApiModelProperty("手动输入成品实体类")
    List<FinishedProductOutDTO> finishedProductOutDTOS;
}
