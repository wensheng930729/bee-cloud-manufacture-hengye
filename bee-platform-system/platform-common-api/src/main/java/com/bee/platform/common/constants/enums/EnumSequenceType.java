package com.bee.platform.common.constants.enums;


public class EnumSequenceType {

    //序列值
    public enum Sequence {
        businessSeq("businessSeq", "业务ID序列"),
        receipt_seq("receipt_seq", "销售收款单ID序列"),
        opening_inventory_seq("opening_inventory_seq", "期初库存单ID序列"),
        out_of_stock_seq("out_of_stock_seq", "领料出库单ID序列"),
        warehousing_seq("warehousing_seq", "成品入库单ID序列"),
        ;
        private String key;
        private String value;

        Sequence() {
        }

        Sequence(String key, String value) {
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
