package com.bee.platform.cloud.si.manufacture.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 报表类型枚举
 *
 * @author Raphael.dq
 * @date 2019/05/29
 */
@Getter
@AllArgsConstructor
public enum EnumConfigReportType {

    /**
     * 库存
     */
    STOCK("stock", "库存"),

    /**
     * 质检
     */
    QUALITY_TEST("quality_test", "质检"),

    /**
     * 采购
     */
    PURCHASE("purchase", "采购"),

    /**
     * 销售
     */
    SALE("sale", "销售"),

    /**
     * 产量分析
     */
    THROUGHPUT_ANALYSIS("throughput_analysis", "产量分析"),

    /**
     * 合格率
     */
    PASS_RATE("pass_rate", "合格率"),

    /**
     * 产量、消耗分析
     */
    OUTPUT_AND_CONSUMPTION_ANALYSIS("output_and_consumption_analysis", "产量、消耗分析"),

    /**
     * 物流
     */
    LOGISTICS("logistics", "物流"),


    ;

    private String code;
    private String value;

}
