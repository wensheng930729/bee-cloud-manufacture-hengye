package com.bee.platform.common.constants.enums;

/**
 * 贷前 贷中 贷后
 */
public class EnumApplyType {

    public enum ApplyType {
        receipt(0, "销售收款"),
        opening_inventory(1, "期初库存"),
        out_of_stock(2, "领料出库"),
        warehousing(3, "成品入库"),
        ;
        private Integer key;
        private String value;

        ApplyType() {
        }

        ApplyType(Integer key, String value) {
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
