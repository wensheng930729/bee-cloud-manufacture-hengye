package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName: LookBoarOutStorageDTO
 * @Description: 看板出库情况数据的返回实体类
 * @Author: fei.sun
 * @Date: 2019/10/28 15:21
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("看板出库情况数据的返回实体类")
public class LookBoarOutStorageDTO {

    @ApiModelProperty("柱状图的X坐标值")
    private String key;

    @ApiModelProperty("纵坐标数据")
    private List<RawMaterialDTO> data;

}
