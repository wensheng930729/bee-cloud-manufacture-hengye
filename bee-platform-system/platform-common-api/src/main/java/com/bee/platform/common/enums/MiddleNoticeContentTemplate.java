package com.bee.platform.common.enums;

import lombok.Getter;

/**
 * @description: 管理中台系统通知内容模板
 * @author: liliang
 * @create: 2019-05-010 11:11
 **/
@Getter
public enum MiddleNoticeContentTemplate {

    //中台部分
    REGISTRATION_COMPLETION(1,"完成注册"),
    PRODUCT_OPENING_APPLY(2,"产品开通申请"),
    PRODUCT_OPENING_APPLY_PASS(3,"产品开通审批通过"),
    PRODUCT_OPENING_APPLY_REFUSE(4,"产品开通审批未通过"),
    ENTERPRISE_APPLY(5,"企业申请"),
    ENTERPRISE_APPLY_PASS(6,"企业申请审批通过"),
    ENTERPRISE_APPLICATION_REFUSE(7,"企业申请审批未通过"),
    ENTERPRISE_RELATE_APPLY(8,"企业关联申请"),
    ENTERPRISE_RELATE_APPLY_PASS(9,"企业关联申请审批通过"),
    ENTERPRISE_RELATE_APPLY_REFUSE(10,"企业关联申请审批未通过"),
    WORK_ORDER_CREATE(11,"提交新工单"),
    WORK_ORDER_RESPONSED(12,"工单状态变化"),
    WORK_ORDER_SOLVED(13,"工单关闭"),
    ENTERPRISES_ADD_USER_MANAGER (14,"通知企业管理员添加企业成员成功"),
    ENTERPRISES_ADD_USER(15,"通知企业成员被添加成功"),
    ENTERPRISE_ADMINISTRATORS_ENABLE_ACCOUNT(16,"企业管理员启用账号"),
    ENTERPRISE_ADMINISTRATORS_DISABLE_ACCOUNT(17,"企业管理员禁用账户"),
    ACCOUNT_ENABLE(18,"账号被企业管理员启用。"),
    ACCOUNT_DISABLE(19,"账号被企业管理员禁用。"),
    BACK_ADMINISTRATOR_ENABLE_ACCOUNT(20,"账号被后台管理员启用。"),
    BACK_ADMINISTRATOR_DISABLE_ACCOUNT(21,"账号被后台管理员禁用。"),

    RESET_PASSWORD(22,"企业管理员重置密码。"),
    ENTERPRISE_ADMIN_RESET_PASSWORD_NOTICE(23,"账号密码被企业管理员重置"),
    ENTERPRISE_ADMIN_RESET_PASSWORD(24,"账号密码被企业管理员重置，并通知新密码"),
    UPDATE_USER_PASSWORD(25,"密码修改成功"),
    NOTICE_USER_RECEIVE_PHONE_CODE(26,"通知手机已发送验证码"),
    SEND_PHONE_CODE(27,"发送手机验证码"),
    UPDATE_USER_PHONE(28,"手机号修改完成"),
    UPDATE_USER_EMAIL(29,"邮箱修改完成"),

    UPDATE_USER_INFO_SUCCESS(30,"修改基本资料完成"),
    UPDATE_USER_INFO(31,"通知管理员用户资料已变更"),
    ENTERPRISE_ADMIN_UPDATE_USER_INFO(32,"通知企业成员，资料已被企业管理员变更"),
    BACKGROUND_ADMIN_UPDATE_USER_INFO(33,"通知企业成员，资料已被后台管理员变更"),
    ENTERPRISE_ADMIN_UPDATE_ENTERPRISES_INFO(34,"修改企业信息完成"),
    ENTERPRISE_ADMIN_ADD_DEPARTMENT(35,"添加了部门"),
    ENTERPRISE_ADMIN_DELETE_DEPARTMENT(36,"删除了部门"),
    ENTERPRISE_ADMIN_ADD_POST(37,"添加职位"),
    ENTERPRISE_ADMIN_DELETE_POST(38,"删除了职位"),

    OPINION_TYPE_COMMITED(39,"意见已提交"),
    OPINION_TYPE_VIEWED(40,"建议意见已被管理员查看"),
    TO_DO_ITEM_GENERATION(41,"新待办生成"),
    ENTERPRISE_ADMIN_EDIT_DEPARTMENT(42, "编辑了部门")

    ;

    private int key;

    private String value;

    MiddleNoticeContentTemplate(){

    }

    MiddleNoticeContentTemplate(int key, String value) {
        this.key = key;
        this.value = value;
    }
}
