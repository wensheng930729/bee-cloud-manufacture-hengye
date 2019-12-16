package com.bee.platform.cloud.si.manufacture.rq;

import cn.afterturn.easypoi.excel.annotation.ExcelTarget;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @ClassName: SampleCodeRQ
 * @Description: 样品编码导出RQ
 * @Author: liliang
 * @Date: 2019/9/30
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ExcelTarget("SampleCodeRQ")
@ApiModel(value = "样品编码导出RQ")
public class SampleCodeRQ {

    @ApiModelProperty("1:样品编码,2：吨袋编码")
    private Integer codeType;

    @ApiModelProperty("序号长度")
    private Integer length;

}
