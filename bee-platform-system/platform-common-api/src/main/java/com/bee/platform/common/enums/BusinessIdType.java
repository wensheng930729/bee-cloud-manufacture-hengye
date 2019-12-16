package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 生成业务id的业务类型
 * @author: junyang.li
 * @create: 2019-04-25 15:11
 **/
@Getter
public enum BusinessIdType {
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
    SUPPLEMENTARY_AGREEMENT("04","agreement"),

    /**
     * 结算单
     */
    STATEMENTS("05","statement"),

    /**
     * 企业
     */
    ENTERPRISE("06","enterprise"),

    ;
    BusinessIdType(){

    }

    BusinessIdType(String code, String desc) {
        this.code = code;
        this.desc = desc;
    }

    private String code;

    private String desc;


}
