package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 资源类型
 * @author: junyang.li
 * @create: 2019-05-13 10:07
 **/
@Getter
public enum  MResourceType {

    /**
     * 菜单
     */
    MENU(1),
    /**
     * 页面
     */
    PAGE(2),
    /**
     * 按钮
     */
    BUTTON(3),
    /**
     * 接口
     */
    INTERFACE(4);

    private int key;

    MResourceType(){

    }

    MResourceType(int key) {
        this.key = key;
    }
}
