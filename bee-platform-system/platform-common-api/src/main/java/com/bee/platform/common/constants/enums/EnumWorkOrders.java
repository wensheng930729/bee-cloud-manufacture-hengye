package com.bee.platform.common.constants.enums;

/**
 * @ClassName EnumWorkOrders
 * @Description 工单枚举类
 * @author qhwang
 */
public class EnumWorkOrders {

    /**
     * 优先级
     */
    public enum PRIORITY{

        /**
         * 重要
         */
        important(2,"重要"),
        /**
         * 一般
         */
        commonly(1,"一般")
        ;

        PRIORITY() {
        }

        PRIORITY(Integer key, String value) {
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
    }

    /**
     * 优先级
     */
    public enum WORK_ORDER_STATUS{

        /**
         * 待平台处理
         */
        todo(1,"待平台处理"),
        /**
         * 平台处理中
         */
        doing(2,"平台处理中"),
        /**
         * 待用户确认
         */
        confirm(3,"待用户确认"),
        /**
         * 已关闭
         */
        close(4,"已关闭")
        ;

        WORK_ORDER_STATUS() {
        }

        WORK_ORDER_STATUS(Integer key, String value) {
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
    }

    /**
     * 回复类型
     */
    public enum REPLY_TYPE{

        /**
         * 提交人
         */
        submitter(1,"提交人"),
        /**
         * 受理人
         */
        receiver(2,"受理人")
        ;

        REPLY_TYPE() {
        }

        REPLY_TYPE(Integer key, String value) {
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
    }

}
