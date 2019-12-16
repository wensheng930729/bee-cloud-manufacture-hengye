package com.bee.platform.common.enums;

/**
 * @Author : chenxm66777123
 * @Description 业务id枚举
 * @Date : 2019/1/8 17:22
 **/
public class EnumGenerateIdModule {

    /**
     * 业务类型id 4位（
     * 前两位区分业务线 10 采购 11 销售 12 生产
     * 后两位根据自己业务自定义例如立项报告相关 20
     * 前后组成业务类型id
     * 例如 采购合同 1020
     * 组成后自己建立枚举值
     */
    public enum Module {
        /**  ************************************* 采购 ************************************* **/
        /**
         * 采购
         */
        BUY_CONTRACT(1001, "采购合同"),
        BUY_PAYMENT(1002, "采购付款"),
        BUY_SETTLE(1003, "采购结算"),
        LOGISTICS_BATCH(1004, "物流批次"),
        TRANSPORT_SECTION(1005, "物流批次运输段"),
        CARRIER_TRANSPORT(1006, "物流批次运输段承运商"),
        /**
         * 采购-磅房信息
         */
        BUY_WEIGHT_MACHINE(1007, "采购磅房信息"),

        /**  ************************************* 销售 ************************************* **/
        SALE_CONTRACT(1101, "采购合同"),
        SALE_COLLECTION(1102, "销售回款"),
        SALE_SETTLE(1103, "销售结算"),
        SALE_LOGISTICS_BATCH(1104, "物流批次"),
        SALE_TRANSPORT_SECTION(1105, "物流批次运输段"),
        SALE_CARRIER_TRANSPORT(1106, "物流批次运输段承运商"),
        SALE_CARRIER_TRANSPORT_DETAIL(1107, "物流批次运输段承运商车次"),


        /**
         * 销售-磅房信息
         */
        SALE_WEIGHT_MACHINE(1105, "采购磅房信息"),
        ;
        private Integer key;
        private String value;

        Module() {
        }

        Module(Integer key, String value) {
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
