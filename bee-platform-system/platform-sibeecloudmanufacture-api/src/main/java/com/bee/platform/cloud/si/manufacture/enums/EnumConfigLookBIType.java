package com.bee.platform.cloud.si.manufacture.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 看板BI类型枚举
 *
 * @author Raphael.dq
 * @date 2019/05/29
 */
@Getter
@AllArgsConstructor
public enum EnumConfigLookBIType {

    /**
     * 数据概览
     */
    DATA_OVERVIEW("data_overview", "数据概览"),

    /**
     * 采购
     */
    PURCHASE("purchase", "采购"),

    /**
     * 生产
     */
    PRODUCE("produce", "生产"),

    /**
     * 销售
     */
    SALE("sale", "销售"),



    /**
     * 库存
     */
    STOCK("stock", "库存"),

    ;

    private String code;
    private String value;

}
