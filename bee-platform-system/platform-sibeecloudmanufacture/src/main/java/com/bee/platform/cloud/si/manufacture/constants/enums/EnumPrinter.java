package com.bee.platform.cloud.si.manufacture.constants.enums;

/**
 * @Description 打印机枚举
 * @author jie.zhang
 * @Date
 * @version 1.0.0
 */
public class EnumPrinter {

    public enum URL_TYPE {
        TOKEN(1, "token"), BASIC_INFO(2, "基础信息"), STATUS(3, "状态"), PRINT(4, "打印");
        private Integer key;
        private String value;

        URL_TYPE() {
        }

        URL_TYPE(Integer key, String value) {
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
     * 映美云返回状态
     */
    public enum JOLI_MARK_STATUS {
        SUCCESS("0", "成功"), TOKEN_INVALID("10102", "token无效"), TOKEN_OVERTIME("10103", "token超时"), SYSTEM_ERROR("90100", "第三方系统错误");
        private String key;
        private String value;

        JOLI_MARK_STATUS() {
        }

        JOLI_MARK_STATUS(String key, String value) {
            this.key = key;
            this.value = value;
        }

        public void setKey(String key) {
            this.key = key;
        }

        public String getKey() {
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
     * 模板一
     */
    public enum TEMPLATE_FIRST{
        TEMPLATE_ID("模板ID","65870813141efe63"),PRINTER_ID("printer_id","19110005L8"),
        APP_ID("app_id","190924185613783"),APP_KEY("app_key","9nffkmbr8b4xknmn"),
        UNIT_TYPE_BUY("BUY","发货单位"),UNIT_TYPE_SALE("SALE","收货单位");
        private String key;
        private String value;

        TEMPLATE_FIRST() {
        }

        TEMPLATE_FIRST(String key, String value) {
            this.key = key;
            this.value = value;
        }

        public void setKey(String key) {
            this.key = key;
        }

        public String getKey() {
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
     * 是否已结算
     */
    public enum IS_SETTLE {

        //未结算
        NO(0, "未结算"),

        //已结算
        YES(1, "已结算");
        private Integer key;
        private String value;

        IS_SETTLE() {
        }

        IS_SETTLE(Integer key, String value) {
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

}
