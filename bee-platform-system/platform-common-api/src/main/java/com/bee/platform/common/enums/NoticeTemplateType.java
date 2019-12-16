package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 管理后台系统通知类型
 * @author: junyang.li
 * @create: 2019-05-06 11:11
 **/
@Getter
public enum NoticeTemplateType {

    //后台部分
    SYSTEM_NOTICE_NOTE(1,"系统通知发生短信验证码"),
    UPDATE_PHONE_NUM(3,"自主修改手机完成"),
    UPDATE_EMAIL(4,"自主修改邮箱完成"),
    USER_UPDATE_PASSWORD(5,"用户完成修改密码"),
    USER_UPDATE_INFORMATION(6,"自主修改其他个人资料完成"),
    ENTERPRISE_APPLY(7,"企业入驻申请"),
    ENTERPRISE_APPROVAL(8,"企业入驻审批"),
    UPDATE_ENTERPRISE_MANAGER(9,"企业管理员变更,自己"),
    UPDATE_ENTERPRISE_MANAGER_OLD(10,"企业管理员变更,旧企业管理员"),
    UPDATE_ENTERPRISE_MANAGER_NEW(11,"企业管理员变更,新企业管理员"),
    PRODUCT_OPEN_APPLY(12,"产品开通申请"),
    PRODUCT_OPEN_APPROVAL(13,"产品开通审批"),
    MANAGER_PROHIBIT(14,"管理员启用/禁用用户账户"),
    MANAGER_UPDATE_USER(15,"编辑用户资料完成"),
    DATA_CONFIGURE(16,"展示数据配置"),
    WORK_ORDER_APPLY(17,"工单提交"),
    WORK_ORDER_REPLY(18,"工单回复"),
    WORK_ORDER_CLOSE(19,"工单关闭"),
    RESET_PASSWORD(20,"管理员重置后台账号密码,操作人"),
    RESET_PASSWORD_USER(21,"管理员重置后台账号密码，被操作人"),
    UPDATE_PERMISSION_GROUP(23,"权限组修改"),
    UPDATE_AUTH_USER(24,"用户权限修改"),
    SHORT_MESSAGE_RESET_PASSWORD(25,"短信接受重置密码"),
    ENTERPRISE_INFO_UPDATE(26,"修改企业信息"),
    ENTERPRISE_INFO_UPDATE_RESULT(27,"修改企业信息"),
    RESET_PASSWORD_BY_EMAIL(28,"管理员通过邮箱重置后台账号密码，被操作人"),

    //中台部分
    PRODUCT_OPENED_AUDIT_APPROVED(51,"产品开通审核通过"),
    PRODUCT_OPENED_AUDIT_FAILED(52,"产品开通审核未通过"),
    ENTERPRISE_APPLICATION_AUDIT_APPROVED(53,"企业申请审核通过"),
    ENTERPRISE_APPLICATION_AUDIT_FAILED(54,"企业申请审核未通过"),
    ENTERPRISE_ASSOCIATION_AUDIT_APPROVED(55,"企业关联申请审核通过"),
    ENTERPRISE_ASSOCIATION_AUDIT_FAILED(56,"企业关联申请审核未通过"),
    WORK_ORDER_REPLIED(57,"工单回复"),
    ENTERPRISE_ADD_USER(58,"企业完成添加用户"),
    ENTERPRISE_ADMIN_ENABLE_ACCOUNT(59,"企业管理员启用账户"),
    ENTERPRISE_ADMIN_DISABLE_ACCOUNT(60,"企业管理员禁用账户"),
    BACKGROUND_ADMIN_ENABLE_ACCOUNT(61,"后台管理员启用账户"),
    BACKGROUND_ADMIN_DISABLE_ACCOUNT(62,"后台管理员禁用账户"),
    ENTERPRISE_ADMIN_RESET_PASSWORD(63,"企业管理员重置密码"),
    SEND_SMS_CODE(64,"发送验证码"),
    UPDATE_USER_EMAIL(65,"修改邮箱完成"),
    UPDATE_USER_INFO(66,"用户资料变更"),
    ENTERPRISE_ADMIN_UPDATE_USER_INFO(67,"用户资料已被企业管理员变更"),
    BACKGROUND_ADMIN_UPDATE_USER_INFO(68,"用户资料已被后台管理员变更"),
    TO_DO_ITEM_GENERATION(69,"新待办生成"),
    ;

    private int key;

    private String value;

    NoticeTemplateType(){

    }

    NoticeTemplateType(int key, String value) {
        this.key = key;
        this.value = value;
    }
}
