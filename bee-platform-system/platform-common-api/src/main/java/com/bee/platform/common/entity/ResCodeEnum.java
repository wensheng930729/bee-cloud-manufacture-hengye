package com.bee.platform.common.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author zhigang.zhou
 * @version 1.0.0
 * @ClassName ResCodeEnum
 * @Description (1)、code为0时：
 * 0->请求成功，程序可以继续往下执行；
 * (2)、code为正数时：
 * 1到20->给tost提示，程序可以继续往下执行；
 * 21到40：弹确认框，用户点击确认之后程序可以继续往下执行；
 * 41到60：跳转业务逻辑页面。
 * (3)、code为负数时：
 * 00到0->弹窗错误提示框，并阻止程序继续往下执行；
 * -20到01->跳转错误页面
 * @Date 2018年10月26日 下午1:16:35
 */
@Getter
@AllArgsConstructor
public enum ResCodeEnum {

    /**
     * 返回枚举
     */
    SUCCESS(1, "成功"),
    ERROR_SUBMIT(0, "提交出错,请联系管理员"),
    ERROR_DATA_EXISTED(0, "已存在id对应数据"),
    ERROR_NOT_FOUND(0, "无法找到相应的数据"),
    BUSY_SYSTEM(0, "系统繁忙,请稍后再试"),
    ERROR_SYSTEM(0, "系统内部错误"),
    FAILED(0, "失败"),
    NO_AUTHORITY(-10, "没有权限"),
    ROLE_NOT_FOUND(-1, "角色不存在"),
    NOT_SELECT_ROLE(-1, "没有权限选择非本角色"),
    ROLE_EXIST(-1, "角色名称重复"),
    NOT_DELETE_ROLE(-1, "该角色下存在用户，无法删除"),
    USER_ALREADY_EXIST(0, "用户已存在"),
    USER_NOT_EXIST(0, "用户不存在"),
    ERROR_PARAMETER(0, "参数错误"),
    ERROR_PARAMETER_NO_COMPANY(0, "参数错误,没有获取到公司id"),
    PARAMETER_INCOMPLETE(0, "参数不全"),
    HYSTRIX_ENABLED(-100, "无法找到相应的数据，该服务被熔断"),
    UPDATE_FAIL(0, "更新失败"),
    UPDATE_FAIL_AND_NOT_FOUND_DATA(0, "更新失败,无法找到相应的数据"),
    UPDATE_SUCCESS(1, "更新成功"),
    SAVE_FAIL(0, "保存失败"),
    SAVE_SUCCESS(1, "保存成功"),
    NOT_CONFIRM_TWICE(0, "请勿重复确认"),
    SURE_FAIL(0, "确认失败"),
    NO_DATA(0, "无法找到对应的数据"),
    DELETE_FAIL(0, "删除失败"),
    USER_NOT_AUTHORIZE(0, "用户未授权,请与管理员联系"),
    MISS_NECESSARY_PARAM(0, "缺少必要参数"),
    NOT_UPLOAD_WEIGHT(0, "未上传磅单信息"),
    IN_WEIGHT_ZERO(0, "进厂称重未填写或小于等于Okg"),
    OUT_WEIGHT_ZERO(0, "出厂称重未填写或小于等于Okg"),
    NET_WEIGHT_ZERO(0, "净重小于等于Okg"),
    CONFIRM_FAILED(0, "确认失败"),
    BAGGING_USED(0, "该吨袋已经被使用过"),
    NOT_HAVE_PRODUCT_ATTR(0, "该产品无化验配置，无需通知取样"),
    NOT_FOUND_PRODUCT_SPEC(0, "未找到该产品对应的产品规格"),
    PASSWORD_ERROR(0, "密码强度不足，密码需要包含数字和字母，至少6位"),
    DATASOURCE_ERROR(0, "数据来源错误！不是新增称重"),
    /**
     * 企业相关
     */
    DEPARTMENT_USER_NOT_EXIST(0, "用户无相关部门"),
    NO_ENTERPRISE_ID(0, "从header中未获取到企业id"),
    ONLY_SUB_ENTERPRISE(0, "管理员不可删除自己管理的母公司，只可删除子公司"),
    ENTERPRISE_NOT_EXIST(0, "企业不存在"),
    ENTERPRISE_EXIST(0, "企业已注册"),
    CANCEL_FAIL(0, "取消认证失败"),
    ENTERPRISE_IN_AUDIT(0, "企业在审核中"),
    ENTERPRISE_USER_REPEATED_AUDIT(0, "企业用户重复审核"),
    ENTERPRISE_REPEAT(0, "企业重复"),
    ENTERPRISE_ASSOCIATED_IN_AUDIT(0, "企业关联正在审核中"),
    ENTERPRISE_ALREADY_ASSOCIATED(0, "企业已关联"),
    ENTERPRISE_IS_OK(1, "企业可关联"),
    ENTERPRISE_REGISTERED_IS_OK(1, "企业可注册"),
    DEPARTMENT_NOT_EXIST(0, "部门不存在"),
    POST_NOT_EXIST(0, "职位不存在"),
    DEPARTMENT_SAVE_FAILED(0, "企业部门信息保存异常"),
    POST_SAVE_FAILED(0, "职位信息保存异常"),
    ROOT_DEPARTMENT_DELETE_FAILED(0, "删除失败，根部门不能删除"),
    DEPARTMENT_CANNOT_BE_EMPTY(0, "删除失败，该部门不为空"),
    PARAMETER_MUST_GT_TWO(0, "参数必须大于等于两个"),
    GET_USER_FAILED(0, "用户查询失败"),
    UPDATE_USER_FAILED(0, "用户编辑失败"),
    GET_ENTERPRISE_USER_RELATION_FAILED(0, "用户企业信息查询失败"),
    GET_DEPARTMENT_USER_RELATION_FAILED(0, "用户部门信息查询失败"),
    USER_AT_ALREADY(0, "该用户已在企业和部门中"),
    USER_AT_OTHER(0, "该用户已在其他集团企业和部门中"),
    USER_AT_ALREADY_ENTERPRISE(0, "该用户已在列表中"),
    ENTERPRISE_CHECK_NOT_EXIST(0, "企业认证单不存在"),
    ENTERPRISE_USER_NOT_FOUND(0, "用户不是该企业成员"),
    NOT_THE_ENTERPRISE_USER(0, "非本企业成员,无法获得用户信息"),
    USER_ROLR_SAVE_FAILED(0, "用户角色信息保存异常"),
    EMAIL_ERROR(0, "请输入正确的邮箱地址"),
    NOT_JOIN_ENTERPRISE(0, "用户未加入任何企业"),
    ADD_FAILED(0, "新增失败"),
    PROHIBIT_ACCOUNT(-1, "该账号被禁用"),
    NOT_PROHIBIT_OWN(-1, "无法禁用自己"),
    NOT_UPDATE_ADMIN(-1, "管理员不允许被修改"),
    /**
     * 平台用户统一code
     */
    NOT_PHONE_NUMBER(0, "请输入正确的手机号"),
    CODE_NOT_NULL(0, "验证码不能为空"),
    VALIDATE_CODE_EXPIRE(0, "验证码已过期，请重新发送"),
    VALIDATE_CODE_ERROR(0, "验证码错误"),
    PHONE_NUMBER_ATYPISM(0, "输入的手机号与本账号不一致，请重新输入"),
    NOT_VALIDATE(0, "验证码未成功验证"),
    USERNAME_OR_PASSWORD_ERROR(0, "账号或密码错误"),
    SEND_MESSAGE_SUCCESS(1, "验证码发送成功"),
    SEND_MESSAGE_FAIL(0, "验证码发送失败,请稍后重试"),
    FAILED_TO_GET_USER_INFO(401, "登录信息已过期，请重新登录"),
    FAILED_TO_GET_CUSTOMER(0, "查询客户信息失败"),
    FAILED_TO_GET_SUPPLIER(0, "查询供应商信息失败"),
    FINANCE_TOKEN_IS_NULL(-2, "登录凭证有误，请重新登录"),
    NOT_FOUND_USERNAME(-3, "未获得用户账号"),
    SAVE_FAILED(0, "保存失败"),
    UPDATE_FAILED(0, "修改失败"),
    DELETE_FAILED(0, "删除失败"),
    NOT_LOGIN(401, "未登录,请先登录"),
    SYS_TOKEN_NOT_NULL(0, "登录凭证不能为空"),
    NOT_FOUND_HEAD_URL(0, "头像地址为空"),
    KEY_WORD_NOT_FOUND(0, "关键词不能为空"),
    NOT_FOUND_USERINFO(401, "获取用户信息失败"),
    ACCOUNT_EXIST(0, "账号已经存在"),
    /**
     * app相关的
     */
    USER_ID_APP_ID_EMPTY(-4, "用户id和应用id不能为空"),

    TEST_ACCOUNT_CANNOT_LOGIN(-2, "测试账号暂不允许登陆"),
    ACCOUNT_NOT_FOUND(-2, "账号未注册，请先注册"),
    LOGIN_FAIL(-2, "账号或密码错误,登录失败"),
    OLD_PASSWORD_SAME_NEW(0, "密码不能与旧密码相同"),
    OLD_PASSWORD_FAIL(0, "旧密码不正确，请重新输入"),
    USER_NOT_IN_ENTERPRISE(0, "用户不在当前企业中，无法切换企业"),
    NOT_FOUND_USER_ROLE(0, "数据异常，未获得用户角色信息"),
    AUDIT_TYPE_ERROR(0, "审核类型错误"),
    AUDIT_COMMENT_ERROR(0, "审核信息为空"),
    UPDATA_FAIL(0, "审核失败"),
    TRADE_ERROR(0, "解析贸易平台返回接口错误，调用失败"),
    LEARN_TYPE_ERROR(0, "学习指南类型错误"),
    INSERT_LEARN_FAILED(0, "学习指南新增失败"),
    UPDATE_LEARN_FAILED(0, "学习指南修改失败"),
    DELETE_LEARN_FAILED(0, "学习指南删除失败"),
    LEARN_NOT_FOUND(0, "没有找到相关学习指南"),
    ERROR_FIELD_TYPE(0, "下料斗类型只能选择料批和漏斗"),

    /**
     * 接口相关
     */
    INTERFACE_NOT_EXIST(0, "接口不存在"),
    INTERFACE_ID_EMPTY(0, "接口id不能为空"),
    INTERFACE_PARAM_EMPTY(0, "接口参数不能为空"),
    INTERFACE_EXISTS(0, "数据已存在"),

    /**
     * 平台用户相关
     */
    PLATFORM_USER_ID_EMPTY(0, "用户id不能为空"),
    PLATFORM_USER_NOT_EXIST(0, "平台用户不存在"),
    UPDATE_PLATFORM_USER_STATUS(0, "修改平台用户状态失败"),
    PLATFORM_USER_DISABLE(0, "用户已被禁用"),
    EMAIL_NOT_RESET_PASSWORD(0, "该用户邮箱为空，不能使用邮箱重置密码"),


    /**
     * 砂石相关
     */
    CUSTOMER_NOT_FORBIDDEN(0, "客户未停用，不能删除"),
    NO_SELECT_RECORD(0, "未选择记录"),
    SALE_ORDER_ID_EMPTY(0, "销售合同id不能为空"),
    SALE_ORDER_EXIST_PAYMENT(0, "不能删除已回款的销售合同"),
    SALE_ORDER_EXIST_SETTLEMENT(0, "不能删除已结算的销售合同"),
    SALE_ORDER_EXIST_INSPECTION_GOODS(0, "不能删除已验货的销售合同"),
    DINAS_PURCHASE_PAY_NOT_ID(0, "采购付款id错误"),
    DINAS_PURCHASE_INVOICE_NOT_ID(0, "采购发票id错误"),
    DINAS_SALE_ORDER_PRODUCT_REPEAT(0, "产品和规格重复"),
    DINAS_SALE_ORDER_EXIST(0, "销售合同号已存在"),
    DINAS_SALE_PAYMENT_EXIST(0, "销售回款单号已存在"),
    NOT_FOUND_PRODUCT(0, "未找到产品信息"),
    /**
     * 样品相关
     */
    SMAPLE_NOT_FOUND(0, "该样品不存在"),
    SMAPLE_BELONG_TO_SALE(0, "销售样品"),
    CODE_EXISTS(0, "样品编码已存在"),
    SMAPLE_SURE_FAIL(0, "出质检单失败"),
    SMAPLE_ROBACK_FAIL(0, "重新检测失败"),
    CARSAMPLE_NOT_FOUND(0, "车次货物确认失败"),
    UNKNOWN_BUSINESS(0, "未知业务线"),
    CODE_NOT_EXIST(0, "编码不存在"),
    CODE_NO_MEG(0, "编码下无信息"),
    CODE_ALREADY_USED(0, "编码已使用"),
    ORE_FURNACE_RECORD_NOT_FOUND(0, "矿热炉记录未找到"),
    NOT_IN_PREPARE_ASSAY(0, "该条码中样品状态未在化验中"),


    /**
     * 样品磅单相关
     */
    WEIGHT_MACHINE_NOT_FOUND(0, "对应磅单未找到"),
    SAMPLE_NOT_FOUND(0, "对应样品未找到"),
    RECORD_NOT_FOUND(0, "记录未找到"),
    ALREADY_PRO_SAMPLE(0, "当前炉号班次炉次已取样且未弃用"),
    FORMULA_ERROR(0, "计算错误,请检查公式是否正确"),

    /**
     * 合同
     */
    CONTRACT_NUMBER_EXISTED(0, "合同编号已存在"),
    CONTRACT_NOT_FOUND(0, "合同未找到"),
    FIELD_IS_EXIST(0, "所有字段均已存在，无需重复添加"),

    /**
     * 导出excel
     */
    EXPORT_EXCEL_ERROR(0, "导出excel出错"),

    /**
     * 华辰智通网关相关
     */
    GATEWAY_EXIST(0, "网关编号已经存在，无法新增"),
    NOT_OUR_GATEWAY(0, "网关不存在，无法操作"),
    PLC_ID_NOT_NULL(0, "plc id 不能为空"),
    PLC_NOT_EXIST(0, "PLC 不存在"),
    PLC_FIELD_NOT_FOUND(0, "PLC 下料斗不存在"),
    /**
     * 料批相关
     */
    MATERIAL_BATCH_EXISTED(0, "料批名称已存在"),
    ORE_FURNACE_SAMPLE_NOTICE_EXIST(0, "已通知取样"),

    /***
     * 设备巡检相关
     */
    DEVICE_INSPECTION_NOT_EXISTED(0, "设备巡检结果不存在"),

    /***
     * 库存产品相关
     */
    STORAGE_PRODUCT_NOT_EXISTED(0, "仓库中不存在相关产品"),

    /***
     * 矿热炉记录相关
     */
    ORE_FURNACE_ELECTRODE_ENDLENGTH_NOT_NULL(0, "电极交时长度不能为空"),
    ORE_FURNACE_POWERRECORD_HANDOVER_NOT_NULL(0, "交时动力电不能为空"),

    /**
     * 盘点相关
     */
    INVENTORY_TYPE_NOT_FUND(0,"盘点类别不存在"),
    SELECTION_PRODUCT_TYPE(0,"请勾选产品分类"),
    SELECTION_PRODUCT(0,"请勾选产品"),
    SELECTION_STORAGE(0,"请勾选仓库"),
    NOT_FOUND_ALL(0,"全盘类型未查询到对应的产品或规格信息"),
    NOT_FUND_PRODUCT_BY_TYPE(0,"当前分类未查询到对应的产品或规格信息"),
    NOT_FUND_PRODUCT_BY_ID(0,"当前产品或规格信息不存在"),
    NOT_FUND_PRODUCT_BY_STORAGE(0,"当前仓库未查询到对应的产品或规格信息"),
    INVENTORY_ORDER_NOT_FOUND(0,"盘点单不存在或已失效"),

    ;
    public Integer code;

    public String msg;

}
