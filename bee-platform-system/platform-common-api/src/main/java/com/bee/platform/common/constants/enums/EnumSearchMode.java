package com.bee.platform.common.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumSearchMode
 * @Description 列表搜索mode的枚举
 */

public class EnumSearchMode {
    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    public enum SearchMode{

        username(1,"用户名"),phone(2,"手机号"),enterprise(3,"企业名称"),product(4,"产品名"),all(5,"全部")
        ;

        private Integer key;
        private String value;


    }
}
