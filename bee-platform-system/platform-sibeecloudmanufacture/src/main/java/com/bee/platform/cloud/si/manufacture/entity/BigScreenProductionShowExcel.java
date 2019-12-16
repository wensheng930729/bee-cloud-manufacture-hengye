package com.bee.platform.cloud.si.manufacture.entity;

import cn.afterturn.easypoi.excel.annotation.Excel;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @author xin.huang
 * @description
 * @date 2019/11/18
 */
@Data
@Accessors(chain = true)
public class BigScreenProductionShowExcel {

    /**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
     */
    @Excel(name = "炉号名称", orderNum = "1", type = 1, width = 20)
    private String furnaceName;

    @Excel(name = "产量", orderNum = "2", type = 10, width = 20)
    private BigDecimal production;

    @Excel(name = "占比", orderNum = "3", type = 10, width = 20)
    private BigDecimal proportion;

}
