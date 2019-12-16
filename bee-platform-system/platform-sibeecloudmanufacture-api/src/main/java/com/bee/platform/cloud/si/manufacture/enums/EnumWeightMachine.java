package com.bee.platform.cloud.si.manufacture.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅相关枚举
 * @Date 2019/9/24 9:58
 */
public class EnumWeightMachine {

    /**
     * 称重结果
     */
    @Getter
    @AllArgsConstructor
    public enum IsWeight {
        NO("未称重", 0),
        YES("已称重", 1)
        ;
        private String key;
        private Integer value;
    }


    /**
     * 称重类型
     */
    @Getter
    @AllArgsConstructor
    public enum ConfirmWeightType {
        IN_FACTORY_WEIGHT("进厂称重", 1),
        OUT_FACTORY_WEIGHT("出厂称重", 2)
        ;
        private String key;
        private Integer value;
    }


    /**
     * 磅单类型
     */
    @Getter
    @AllArgsConstructor
    public enum WeightType {
        BUY("采购", 1),
        SALE("销售", 2)
        ;
        private String key;
        private Integer value;
    }


    /**
     * 信息来源 0 物流推送 1 新增称重
     */
    @Getter
    @AllArgsConstructor
    public enum DataSource {
        LOGISTICS_PUSH("物流推送", 0),
        ADD("新增称重", 1)
        ;
        private String key;
        private Integer value;
    }
}
