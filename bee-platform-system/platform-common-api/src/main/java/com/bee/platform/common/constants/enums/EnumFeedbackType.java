package com.bee.platform.common.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumFeedbackType
 * @Description 意见反馈 反馈类型
 */

public class EnumFeedbackType {

    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    public enum adviceType{
        bug(0,"产品BUG"),advice(1,"功能建议")
        ;
        private Integer key;
        private String value;

    }
}
