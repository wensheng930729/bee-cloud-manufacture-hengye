package com.bee.platform.common.enums;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @Classname OperationType
 * @Description 用户操作类型
 * @Date 2019/5/24 16:27
 * @Author xin.huang
 */
@Getter
@NoArgsConstructor
public enum OperationType {
    /**
     * 添加
     */
    ADD(0),
    /**
     * 修改
     */
    UPDATE(1),
    /**
     * 删除
     */
    DELEETE(2);

    private Integer key;

    OperationType(Integer key){
        this.key=key;
    }
}
