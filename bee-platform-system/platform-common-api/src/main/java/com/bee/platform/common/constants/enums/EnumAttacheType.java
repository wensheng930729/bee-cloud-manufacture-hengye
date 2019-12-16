package com.bee.platform.common.constants.enums;

/**
 * @ClassName EnumAttacheType
 * @Description 附件类型
 */

public class EnumAttacheType {

    public enum type{

        enclosure(0,"营业执照"),permit(1,"营业许可证"),certificate(2,"企业认证授权书"),logo(3,"企业logo");

        type() {
        }

        type(Integer key, String value) {
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
