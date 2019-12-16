package com.bee.platform.cloud.si.manufacture.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

public class EnumLookBoardType {

    @Getter
    @AllArgsConstructor
    public enum ParamType {
        MAIN(1, "主料"),
        ACCESSORIE(2, "辅料"),
        PRODUCT(3, "成品"),
        CUSTOMER(4, "交易对象");

        private Integer key;
        private String value;

    }

    @Getter
    @AllArgsConstructor
    public enum ProductCategory {
        MAIN(1, "主料"),
        ACCESSORIE(2, "辅料"),
        PRODUCT(3, "成品"),
        ;

        private Integer key;
        private String value;

    }
}
