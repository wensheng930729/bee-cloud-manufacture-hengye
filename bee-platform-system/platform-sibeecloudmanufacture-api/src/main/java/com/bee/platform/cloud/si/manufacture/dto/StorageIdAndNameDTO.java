package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: StorageIdAndNameDTO
 * @Description: 仓库id和名称DTO
 * @Author: ll
 * @Date: 2019-11-12
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("仓库id和名称DTO")
public class StorageIdAndNameDTO {

    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

}
