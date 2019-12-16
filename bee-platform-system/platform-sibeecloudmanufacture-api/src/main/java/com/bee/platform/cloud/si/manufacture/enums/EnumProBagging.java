package com.bee.platform.cloud.si.manufacture.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author liang.li
 * @ClassName EnumSampleRelation
 * @Description TODO
 * @Date
 */
public class EnumProBagging {

    /**
     * 步骤
     */
    @Getter
    @AllArgsConstructor
    public enum Step {
        ONE(1, "第一步"),
        TWO(2, "第二步")

        ;
        private Integer key;
        private String value;
    }

    /**
     * 班次
     */
    @Getter
    @AllArgsConstructor
    public enum Shift {
        ZERO(0, ""),
        ONE(1, "一班"),
        TWO(2, "二班"),
        THREE(3, "三班")

        ;
        private Integer key;
        private String value;
    }

    @Getter
    @AllArgsConstructor
    public enum SHIFT_AMOUNT {
        ONE(1, "shiftAmount1"),
        TWO(2, "shiftAmount2"),
        THREE(3, "shiftAmount3")

        ;
        private Integer key;
        private String value;
    }
    @Getter
    @AllArgsConstructor
    public enum SHIFT_SUM {
        ONE(1, "shiftSum1"),
        TWO(2, "shiftSum2"),
        THREE(3, "shiftSum3")

        ;
        private Integer key;
        private String value;
    }
    @Getter
    @AllArgsConstructor
    public enum SHIFT_PERCENT {
        ONE(1, "shiftPercent1"),
        TWO(2, "shiftPercent2"),
        THREE(3, "shiftPercent3")

        ;
        private Integer key;
        private String value;
    }
    @Getter
    @AllArgsConstructor
    public enum FURNACE_FIELDS {
        AMOUNT(1, "furnaceAmount"),
        PERCENT(2, "furnacePercent"),
        SUM(3, "furnaceSum")

        ;
        private Integer key;
        private String value;
    }
}
