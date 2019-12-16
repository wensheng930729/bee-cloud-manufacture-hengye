package com.bee.platform.common.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumEnterpriseLevel
 * @Description 公司层级
 */

public class EnumComonRegionLevel {

    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    public enum level{

        zero(0,"国家级"),one(1,"省级"),three(2,"市级"),four(3,"区县级")
        ;

        private Integer key;
        private String value;


    }
}
