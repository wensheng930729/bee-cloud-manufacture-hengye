package com.bee.platform.cloud.si.manufacture.rq;

import com.bee.platform.common.entity.Page;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @description: 盘点单列表查询参数
 * @author: junyang.li
 * @create: 2019-11-27 13:57
 **/
@Data
@NoArgsConstructor
public class InventorySearchRQ implements Serializable {

    private static final long serialVersionUID = 966580746892653513L;

    @ApiModelProperty("列表查询开始时间")
    private String startTime;

    @ApiModelProperty("列表查询结束时间")
    private String endTime;

    @ApiModelProperty("列表查询分页对象")
    private Page page;
}
