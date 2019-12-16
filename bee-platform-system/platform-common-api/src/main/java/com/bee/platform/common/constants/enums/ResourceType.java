package com.bee.platform.common.constants.enums;

import lombok.Getter;

/**
 * @description: 资源类型
 * @author: junyang.li
 * @create: 2019-09-20 16:08
 **/
@Getter
public enum  ResourceType {

    DEFAULT("default"),
    /**
     * 菜单
     */
    MENU("menu"),
    /**
     * 按钮
     */
    BUTTON("button"),
    /**
     * 功能
     */
    FUNCTION("function"),
    /**
     * 页面
     */
    PAGE("page"),
    ;

    private String key;

    ResourceType(String key) {
        this.key = key;
    }
}
