package com.bee.platform.cloud.si.manufacture.constants.enums;

/**
 * @Description 合同相关枚举
 * @author jie.zhang
 * @Date
 * @version 1.0.0
 */
public class EnumContract {

    public enum CONFIRM_PART {
        SELF("0", "我方"), SUPPLIER("1", "供应商");
        private String key;
        private String value;

        CONFIRM_PART() {
        }

        CONFIRM_PART(String key, String value) {
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

    public enum PURCHASER_MODE {
        SELF_MENTION("0", "自提"), SUPPLIER_DELIVERY("1", "供应商");
        private String key;
        private String value;

        PURCHASER_MODE() {
        }

        PURCHASER_MODE(String key, String value) {
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

    public enum SETTLE_STATUS {
        WEIGHT(0, "重量确认"), PRICE(1, "价格确认"), COMPLETED(2, "完成");
        private Integer key;
        private String value;

        SETTLE_STATUS() {
        }

        SETTLE_STATUS(Integer key, String value) {
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

    public enum IS_COMPLETED{

        NO(0,"进行中"),YES(1,"已完成")
        ;

        IS_COMPLETED() {
        }

        IS_COMPLETED(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        private Integer key;

        private String value;

        public Integer getKey() {
            return key;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public static String getValue(Integer key) {
            if(key!=null){
                IS_COMPLETED[] typeNames = values();
                for (IS_COMPLETED typeName : typeNames) {
                    if (typeName.getKey().equals(key)) {
                        return typeName.getValue();
                    }
                }
            }
            return null;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }

}
