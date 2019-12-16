package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName: InStorageRecordDTO
 * @Description: 入库记录列表实体类
 * @Author: fei.sun
 * @Date: 2019/9/30 14:09
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("入库记录列表实体类")
public class InStorageRecordListDTO {

    @ApiModelProperty("采购入库列表实体")
    private List<BuyInStorageRecordDTO> buyInStorageRecordDTOS;

    @ApiModelProperty("产成品入库列表实体")
    private List<FinishProductInStorageRecordDTO> finishProductInStorageRecordDTOS;

    @ApiModelProperty("新增入库列表实体")
    private List<FreeInStorageRecordDTO> freeInStorageRecordDTOS;

}
