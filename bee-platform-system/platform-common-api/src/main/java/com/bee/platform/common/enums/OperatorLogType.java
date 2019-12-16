package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 后台管理系统操作日志类型 Enable
 * @author: junyang.li
 * @create: 2019-05-05 09:19
 **/
@Getter
public enum  OperatorLogType {

    ADD_ROLE("管理员【{0}】新增权限组【{1}】"),DELETE_ROLE("管理员【{0}】删除权限组【{1}】"),EDIT_ROLE("管理员【{0}】修改权限组【{1}】对应的权限"),
    PROHIBIT_ACCOUNT("用户【{0}】被管理员【{1}】禁止登录"),ENABLE_ACCOUNT("用户【{0}】被管理员【{1}】解除禁止"),
    RESET_PASSWORD("管理员【{0}】重置了用户【{1}】的登录密码"),
    CREATE_NEW_ACCOUNT("管理员【{0}】新增用户【{1}】赋予权限【{2}】"),
    UPDATE_USER_INFO("管理员【{0}】修改了【{1}】的用户信息"),
    UPDATE_ROLE("，并将权限组【{0}】更改为【{1}】"),
    ;

    private String desc;

    OperatorLogType(String desc) {
        this.desc = desc;
    }

    OperatorLogType(){

    }
}
