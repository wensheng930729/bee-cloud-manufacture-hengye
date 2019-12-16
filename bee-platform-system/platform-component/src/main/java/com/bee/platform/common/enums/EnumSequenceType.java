package com.bee.platform.common.enums;

/* *
 * @Author : chenxm66777123
 * @Description 业务id枚举
 * @Date : 2019/1/8 17:22
 **/
public class EnumSequenceType {

    //序列值
    public enum Sequence {
        businessSeq("businessSeq", "业务ID序列"),
        orderSeq("orderSeq", "订单ID序列")
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
