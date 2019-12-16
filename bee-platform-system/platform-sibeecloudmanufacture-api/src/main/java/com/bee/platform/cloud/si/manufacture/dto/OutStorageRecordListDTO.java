package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName: OutStorageRecordListDTO
 * @Description: 出库记录列表实体类
 * @Author: fei.sun
 * @Date: 2019/9/30 16:06
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("出库记录列表实体类")
public class OutStorageRecordListDTO {

    @ApiModelProperty("销售出库列表实体")
    private List<SaleOutStorageRecordDTO> saleOutStorageRecordDTOS;

    @ApiModelProperty("领用出库列表实体")
    private List<PickOutStorageDTO> pickOutStorageDTOS;

    @ApiModelProperty("生产出库列表实体")
    private List<ProductionOutStorageDTO> productionOutStorageDTOS;

}
