package com.bee.platform.cloud.si.manufacture.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * 业务类型枚举
 *
 * @author Raphael.dq
 * @date 2019/05/29
 */
@Getter
@AllArgsConstructor
public enum EnumBusinessType {

    /**
     * 单位
     */
    UNIT("unit", "单位"),

    /**
     * 样品类型
     */
    SAMPLE_TYPE("sample_type", "样品类型"),

    /**
     * 班次
     */
    SHIFTS("shifts", "班次"),



    ;

    private String code;
    private String value;

}
