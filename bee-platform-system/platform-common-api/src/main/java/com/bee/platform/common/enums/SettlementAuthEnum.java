package com.bee.platform.common.enums;

/**
 * @description: 结算权限的枚举，临时使用，后面更改
 * @author: junyang.li
 * @create: 2019-11-06 16:23
 **/

public enum  SettlementAuthEnum {
    /**
     * 采购人员
     */
    PURCHASE(1,"maf_purchase_auth"),
    /**
     * 计算人员
     */
    SETTLEMENT(140,"maf_settlement_auth"),
    /**
     * 销售
     */
    SALE(150,"maf_sale_auth"),
    ;

    private Integer key;

    private String value;

    SettlementAuthEnum(Integer key, String value) {
        this.key = key;
        this.value = value;
    }

    /**
     * 通过key 获得对应的对象
     */
    public static SettlementAuthEnum getByKey(Integer key){
        if(key==null){
            return null;
        }
        SettlementAuthEnum[] values=SettlementAuthEnum.values();
        for (SettlementAuthEnum item:values) {
            if(item.key.equals(key)){
                return item;
            }
        }
        return null;
    }

    /**
     * 通过key 获得对应的value
     */
    public static String getValueByKey(Integer key){
        SettlementAuthEnum authEnum=SettlementAuthEnum.getByKey(key);
        return authEnum==null?null:authEnum.value;
    }
}
