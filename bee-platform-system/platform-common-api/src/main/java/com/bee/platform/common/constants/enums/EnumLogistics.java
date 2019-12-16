package com.bee.platform.common.constants.enums;

/**
 * @ClassName EnumLogistics
 * @Description 物流相关枚举
 * @author qhwang
 */
public class EnumLogistics {

    /**
     * 运输方式
     */
    public enum transport_mode{

        /**
         * 汽车
         */
        car(1, "汽车"),
        /**
         * 轮船
         */
        boat(2, "轮船"),
        /**
         * 火车
         */
        train(3, "火车")
        ;

        transport_mode() {
        }

        transport_mode(Integer key, String value) {
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

        public void setValue(String value) {
            this.value = value;
        }

        public static String getValue(Integer key) {
            transport_mode[] values = values();
            for (transport_mode value : values) {
                if (value.getKey().equals(key)) {
                    return value.getValue();
                }
            }
            return null;
        }
    }

    /**
     * 货运是否直接到厂
     */
    public enum to_factory{

        /**
         * 不到厂
         */
        NO(0, "不到厂"),
        /**
         * 到厂
         */
        YES(1, "到厂")
        ;

        to_factory() {
        }

        to_factory(Integer key, String value) {
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

        public void setValue(String value) {
            this.value = value;
        }

        public static String getValue(Integer key) {
            to_factory[] values = values();
            for (to_factory value : values) {
                if (value.getKey().equals(key)) {
                    return value.getValue();
                }
            }
            return null;
        }
    }

    /**
     * 是否已经到厂
     */
    public enum arrival_status{

        /**
         * 未到厂
         */
        NO(0, "未到厂"),
        /**
         * 已到厂
         */
        YES(1, "已到厂")
        ;

        arrival_status() {
        }

        arrival_status(Integer key, String value) {
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

        public void setValue(String value) {
            this.value = value;
        }

        public static String getValue(Integer key) {
            arrival_status[] values = values();
            for (arrival_status value : values) {
                if (value.getKey().equals(key)) {
                    return value.getValue();
                }
            }
            return null;
        }
    }

}
