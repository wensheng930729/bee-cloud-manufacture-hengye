package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName: InStorageSampleDTO
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/8 10:54
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("采购入库详情化验结果")
public class InStorageSampleDTO {

    @ApiModelProperty("样品编号")
    private String sampleCode;

    @ApiModelProperty("样品化验结果")
    private List<AssayItem> assayItems;

}
