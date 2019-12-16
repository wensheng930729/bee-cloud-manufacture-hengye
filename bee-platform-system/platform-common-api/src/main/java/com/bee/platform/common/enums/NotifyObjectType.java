package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 系统消息通知对象的类型
 * @author: junyang.li
 * @create: 2019-05-06 15:43
 **/
@Getter
public enum  NotifyObjectType {

    OPERATOR(1,"操作人"),GIVING_PEOPLE(2,"被操作人"),WORK_ORDER_ROLE(3,"具有工单处理权限的角色"),
    PRODUCT_OPEN_ROLE(4,"具有产品开通审核权限的角色"),ENTERPRISE_APPROVAL(5,"具有企业审核权限的角色"),
    OLD_ENTERPRISE_MANAGER(6,"旧企业管理员"),NEW_ENTERPRISE_MANAGER(7,"新企业管理员")
    ;

    private int key;

    private String value;

    NotifyObjectType(){

    }

    NotifyObjectType(int key, String value) {
        this.key = key;
        this.value = value;
    }
}
