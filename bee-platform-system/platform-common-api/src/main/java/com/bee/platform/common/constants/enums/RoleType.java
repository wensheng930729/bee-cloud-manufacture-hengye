package com.bee.platform.common.constants.enums;

import lombok.Getter;
import org.springframework.util.StringUtils;

/**
 * @description:  角色枚举
 * @author: junyang.li
 * @create: 2019-09-25 15:57
 **/
@Getter
public enum  RoleType {

    /**
     * 超级管理员
     */
    SUPER_ADMIN("super_admin"),
    /**
     * 基础角色
     */
    BASE("base"),
    /**
     * 企业管理员
     */
    ENTERPRISE_ADMIN("enterprise_admin"),
    /**
     * 企业角色
     */
    ENTERPRISE_ROLE("enterprise_role"),
    /**
     * 承运商角色
     */
    CARRIER("carrier"),
    /**
     * 客户角色
     */
    CUSTOMER("customer"),
    ;

    private String code;

    RoleType(String code) {
        this.code = code;
    }
    /**
     * @notes: 判断是否是超级管理员
     * @Author: junyang.li
     * @Date: 16:10 2019/9/25
     * @param type :
     * @return: boolean
     */
    public static boolean isAdmin(String type){
        return ENTERPRISE_ADMIN.code.equals(type);
    }
    /**
     * @notes: 当前角色类型是否是允许编辑的角色类型
     * @Author: junyang.li
     * @Date: 13:16 2019/10/24
     * @param type :
     * @return: boolean
     */
    public static boolean canEdit(String type){
        return BASE.getCode().equals(type) || ENTERPRISE_ROLE.getCode().equals(type);
    }
}
