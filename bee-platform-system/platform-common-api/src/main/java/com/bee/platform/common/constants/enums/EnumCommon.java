package com.bee.platform.common.constants.enums;

/**
 * @ClassName EnumCommon
 * @Description 公用枚举
 */

public class EnumCommon {
    public enum IsActive{
        not_active(0,"未激活"),
        is_active(1,"激活")
        ;

        IsActive() {
        }

        IsActive(Integer key, String value) {
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

    public enum IS_COMPLETED{
        NO(0,"未完成"),YES(1,"已完成")
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

        public void setValue(String value) {
            this.value = value;
        }
    }
    
    public enum LogicStatus{
        DELETED(0,"删除"),
        NORMAL(1,"正常");
        private Integer key;
        private String value;

        LogicStatus() {
        }

        LogicStatus(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

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
