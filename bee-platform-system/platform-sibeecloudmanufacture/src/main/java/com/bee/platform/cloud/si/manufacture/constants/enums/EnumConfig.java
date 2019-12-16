package com.bee.platform.cloud.si.manufacture.constants.enums;

/**
 * @Description 配置枚举
 * @author chenxm66777123
 * @Date 2019/11/27 10:20
 * @version 1.0.0
 */
public class EnumConfig {

    public enum STORAGE_TYPE {

        finished_product(0, "成品"),
        raw_material(1, "原料"),
        mixed_ingredients(2, "配料"),
        hardware(3, "五金"),
        other(4, "其他");

        private Integer key;
        private String value;

        STORAGE_TYPE() {
        }

        STORAGE_TYPE(Integer key, String value) {
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
