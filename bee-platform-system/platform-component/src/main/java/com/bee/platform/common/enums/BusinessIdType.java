package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-04-15 14:36
 **/
@Getter
public enum  BusinessIdType {

    /**
     * 用户
     */
    USER("01","user"),
    /**
     * 订单
     */
    ORDER("02","order"),
    /**
     * 合同
     */
    CONTRACT("03","contract"),
    /**
     * 补充协议
     */
    PROTOCOL("04","protocol"),
    /**
     * 结算单
     */
    SETTLEMENT("05","settlement"),
    /**
     * 企业
     */
    COMPANY("06","company");

    private String code;

    private String value;

    BusinessIdType(){

    }

    BusinessIdType(String code, String value) {
        this.code = code;
        this.value = value;
    }
}
