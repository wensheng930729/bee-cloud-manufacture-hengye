package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName: FinishedProductStorageListDTO
 * @Description: 产成品入库批量入库参数实体类
 * @Author: fei.sun
 * @Date: 2019/9/25 14:03
 * @Version: 1.0
 */
@ApiModel("产成品入库批量入库参数实体类")
@Data
@Accessors(chain = true)
public class FinishedProductStorageListDTO {

    @ApiModelProperty("仓库编号")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("列表内容")
    private List<FinishedProductStorageDTO> finishedProductStorageDTOS;
}
