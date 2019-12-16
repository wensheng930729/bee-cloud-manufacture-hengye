package com.bee.platform.cloud.si.manufacture.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @Description 报表枚举
 * @author jie.zhang
 * @Date
 * @version 1.0.0
 */
public class EnumReportForm {

    /**
     * 报表类型
     */
    public enum REPORT_TYPE {
        EXISTING_DETAILS(1, "现存明细表"), PURCHASE_WAREHOUSING(2, "采购入库表"), MATERIAL_DAILY(3, "原料日报表"),
        PRODUCE_WAREHOUSING(4, "产成品入库"),PRODUCE_CHECKOUT(5, "成品出库"), QUALITY_TEST_PRODUCE(6, "质检"), QUALITY_TEST_IN_OUT(7, "质检"),
        BUY(8, "采购"), SALE(9, "销售"),YIELD_ANALYSIS(10, "产量分析"), PASS_RATE(11, "合格率"), OUTPUT_CONSUMPTION_ANALYSIS(12, "产量消耗分析"), LOGISTICS(13, "物流");
        private Integer key;
        private String value;

        REPORT_TYPE() {
        }

        REPORT_TYPE(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public Integer getKey() {
            return key;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    /**
     * 质检类型 1生产2进出厂
     */
    public enum TEST_TYPE {
        PRODUCE(1, "生产"), IN_OUT_FACTORY(2, "进出厂");
        private Integer key;
        private String value;

        TEST_TYPE() {
        }

        TEST_TYPE(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public Integer getKey() {
            return key;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    /**
     * 报表动态内容衔接标识 1前 2后
     */
    public enum DYNAMIC_CONTENT {
        FRONT(1, "前"), AFTER(2, "后");
        private Integer key;
        private String value;

        DYNAMIC_CONTENT() {
        }

        DYNAMIC_CONTENT(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public Integer getKey() {
            return key;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    @Getter
    @AllArgsConstructor
    public enum QualifiedLine{
        /**
         * 不是合格线
         */
        unqualified(0,"不是合格线"),
        /**
         * 是合格线
         */
        qualified(1,"是合格线");
        private Integer key;
        private String value;
    }

    /**
     * 进出厂质检
     */
    public enum INSPECTION_TYPE {
        PRODUCE(1, "进厂质检"), IN_OUT_FACTORY(2, "出厂质检");
        private Integer key;
        private String value;

        INSPECTION_TYPE() {
        }

        INSPECTION_TYPE(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public Integer getKey() {
            return key;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }

        public static String getValue(Integer key) {
            if(key!=null){
                INSPECTION_TYPE[] typeNames = values();
                for (INSPECTION_TYPE typeName : typeNames) {
                    if (typeName.getKey().equals(key)) {
                        return typeName.getValue();
                    }
                }
            }
            return null;
        }
    }

}
