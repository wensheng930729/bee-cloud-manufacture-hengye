package com.bee.platform.common.constants.enums;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName EnumMiddleNoticeTitle
 * @Description 功能描述
 * @Date 2019/5/10 15:21
 **/

public class EnumMiddleNoticeTitle {

    public enum title{
        WELCOME(0,"欢迎使用金蜜工业云"),
        OPEN_PRODUCT_APPLY(1,"产品开通申请"),
        OPEN_PRODUCT_RESULT(2,"产品开通审批结果"),
        ENTERPRISE_APPLY(3,"企业申请"),
        ENTERPRISE_APPLY_RESULT(4,"企业申请审批结果"),
        ENTERPRISE_RELATION_APPLY(5,"企业关联申请"),
        ENTERPRISE_RELATION_APPLY_RESULT(6,"企业关联审批结果"),
        CREATE_WORK_ORDER(7,"提交新工单"),
        WORK_ORDER_STATUS_CHANGE(8,"工单状态变化"),
        WORK_ORDER_CLOSE(9,"工单关闭"),
        ADD_USER_SUCCESS(10,"成功添加新用户"),
        USER_STATUS_CHANGE(11,"用户状态变更"),
        MANGER_CHANGE_PASSWORD(12,"管理员重置密码"),
        CHANGE_PASSWORD_NOTICE(13,"修改密码通知"),
        WAIT_PHONE_VERIFICATION(14,"等待手机验证"),
        CHANGE_PHONE_NOTICE(15,"修改手机号码通知"),
        CHANGE_EMAIL_NOTICE(16,"修改邮箱通知"),
        CHANGE_BASIC_DATA_NOTICE(17,"修改基本资料通知"),
        USER_CHANGE_DATA_NOTICE(18,"用户资料变更通知"),
        ENTERPRISE_CHANGE_DATA_NOTICE(19,"企业信息变更通知"),
        ADD_DEPARTMENT_NOTICE(20,"添加部门通知"),
        DELETE_DEPARTMENT_NOTICE(21,"删除部门通知"),
        ADD_POSITION_NOTICE(22,"添加职位通知"),
        DELETE_POSITION_NOTICE(23,"删除职位通知"),
        FUNCTIONAL_SUGGESTIONS_SUBMIT(24,"功能建议/BUG已提交"),
        FUNCTIONAL_SUGGESTIONS_READ(25,"功能建议/BUG被查看"),
        NEED_TO_BE_DEALT_WITH(26,"待办需处理"),
        UPDATE_ENTERPRISE_MANAGER(27,"企业管理员变更"),
        ENTERPRISE_USER(28,"企业员工关联申请"),
        ENTERPRISE_USER_RESULT_NO(29,"企业用户关联审核拒绝"),
        EDIT_DEPARTMENT_NOTICE(30,"编辑部门通知")
        ;

        title() {
        }

        title(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        private Integer key;
        private String value;

        public Integer getKey() {
            return key;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }

}
