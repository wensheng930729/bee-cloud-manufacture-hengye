package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName: StorageManagerRecordListDTO
 * @Description: 库存管理出入库列表实体类
 * @Author: fei.sun
 * @Date: 2019/9/29 15:36
 * @Version: 1.0
 */

@ApiModel("库存管理列表实体类")
@Accessors(chain = true)
@Data
public class StorageManagerRecordListDTO {

    @ApiModelProperty("入库记录列表")
    private List<InAndOutStorageRecordDTO> inStorageRecords;

    @ApiModelProperty("出库记录列表")
    private List<InAndOutStorageRecordDTO> outStorageRecords;
}
