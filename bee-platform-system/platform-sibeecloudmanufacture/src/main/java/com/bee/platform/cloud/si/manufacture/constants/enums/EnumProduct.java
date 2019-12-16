package com.bee.platform.cloud.si.manufacture.constants.enums;

import java.math.BigDecimal;

/**
 * @author xin.huang
 * @description 产品相关枚举
 * @date 2019/10/18
 */
public class EnumProduct {

    /**
     * @descriptin 成品/料批主含量
     */
    public enum MAIN_CONTENT {
        FINISH("0.993", "成品"), INGREDIENT("0.9", "主料");
        private String key;
        private String value;

        MAIN_CONTENT() {
        }

        MAIN_CONTENT(String key, String value) {
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
}
