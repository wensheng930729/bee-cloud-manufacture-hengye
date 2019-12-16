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
public class BigScreenMaterialConsumeExcel {
    /**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
     */
    @Excel(name = "炉号id", orderNum = "1", type = 10, width = 20)
    private Integer furnaceId;

    @Excel(name = "炉号名称", orderNum = "2", type = 1, width = 20)
    private String furnaceName;

    @Excel(name = "产品id", orderNum = "3", type = 10, width = 20)
    private Integer productId;

    @Excel(name = "产品名称", orderNum = "4", type = 1, width = 20)
    private String productName;

    @Excel(name = "产品规格id", orderNum = "5", type = 10, width = 20)
    private Integer productSpecId;

    @Excel(name = "产品规格名称", orderNum = "6", type = 1, width = 20)
    private String productSpecName;

    @Excel(name = "消耗数量", orderNum = "7", type = 10, width = 20)
    private BigDecimal amount;
}
