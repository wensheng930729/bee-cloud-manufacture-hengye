package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 资源等级
 * @author: junyang.li
 * @create: 2019-05-13 10:10
 **/
@Getter
public enum  MResourceLev {

    /**
     * 一级
     */
    FRIST(1),
    /**
     * 二级
     */
    SECOND(2);

    private int key;

    MResourceLev(){

    }
    MResourceLev(int key) {
        this.key = key;
    }
}
