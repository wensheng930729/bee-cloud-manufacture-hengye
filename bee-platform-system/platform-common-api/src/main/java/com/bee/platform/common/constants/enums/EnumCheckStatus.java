package com.bee.platform.common.constants.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @ClassName EnumFeedbackType
 * @Description 查看状态
 */

public class EnumCheckStatus {

    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    public enum checkStatus{
        no(0,"未查看"),yes(1,"已查看")
        ;
        private Integer key;
        private String value;

    }
}
