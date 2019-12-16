package com.bee.platform.common.exception;

import lombok.AllArgsConstructor;
import lombok.Getter;


/**
 * @author zhigang.zhou
 * @version 1.0.0
 * @ClassName ExceptionMessageEnum
 * @Description 所有参照业务模块的com.bee.trade.common.entity.ResCodeEnum的值进行 {@code code} 定义相应的错误编码！
 * 每个业务模块的错误使用相应的错误编码区间！详情见示例！
 * @Date 2018年10月26日 下午4:00:47
 */
@Getter
@AllArgsConstructor
public enum ExceptionMessageEnum {

    /**
     * 系统级错误异常
     */
    SUCCESS(0, "成功"),
    SYSTEM_INVALID_PARAMS(1, "参数错误"),
    SYSTEM_NOT_FOUND(10, "无法找到相应的数据"),
    ERROR_SYSTEM(-1, "系统繁忙,请稍后再试"),
    ERROR_DATA(-1, "无法找到相应数据"),

    /**
     * 示例异常：用户模块
     */
    USER_VALUE_EMPTY(20003, "参数不允许为空"),
    USER_NOT_LEGAL(20005, "登录凭证有误，请重新登录"),
    NO_AUTHORITY(20011, "没有权限"),
    FAILED_TO_GET_USER_INFO(401, "登录信息已过期，请重新登录"),
    FAILED_TO_GET_CUSTOMER(401, "获取客户信息失败"),
    FAILED_TO_GET_SUPPLIER(401, "获取供应商信息失败"),
    USER_NOT_AUTHORIZE(20026,"用户未授权,请与管理员联系"),
    SEND_SMS_ERROR(20026,"短信发送失败"),


    CONTRACT_NOT_FOUND(200065, "该合同不存在"),
    VALIDATE_CODE_EXPIRE(200070, "验证码已过期，请重新发送"),
    VALIDATE_CODE_ERROR(200072, "验证码错误"),
    /**
     * app相关
     */
    UPDATE_APP_ID_FAILED(200104, "更新appID失败"),
    USER_OPEN_APP_FAILED(200105, "用户开通应用失败"),
    USER_REMOVE_APP_FAILED(200106, "关闭应用失败"),
    USER_ID_PARAMS_NOT_FOUND(200107, "请求参数异常，请求的USER_ID未匹配到相关企业用户信息"),
    APP_ID_PARAMS_NOT_FOUND(200108, "请求参数异常，请求的APP_ID未匹配到相关应用信息"),
    USER_ID_APP_ID_IS_EMPTY(200109, "请求参数异常，请求的USER_ID或APP_ID为空"),
    ENTERPRISESAPPSLOG_SAVE_FAILED(0, "日志记录保存异常"),

    /**
     * 企业相关
     */
    ENTERPRISE_EXIST(0, "企业已注册"),
    ENTERPRISE_REPEAT(0, "企业名称异常，企业重复"),
    ENTERPRISE_NOT_EXIST(201001, "企业不存在"),
    ADMIN_NOT_FOUND(201004, "企业管理员不存在"),

    /**
     * 用户相关
     */
    USER_NOT_EXIST(202001, "用户不存在"),
    ACCOUNT_NAME_REPEAT(202008, "账户名重复"),
    NOT_VALIDATE(202009, "验证码未成功验证"),
    OLD_PASSWORD_SAME_NEW(202010, "密码不能与旧密码相同"),

    /**
     * 验货磅单相关
     */
    DINAS_INSPECTION_GOODS_SAVE_FAILED(0,"保存验货磅单失败"),
    DINAS_INSPECTION_GOODS_DELETED_FAILED(0,"删除验货磅单失败"),
    DINAS_INSPECTION_GOODS_DELETED_FAILED_NO_DATA(0,"删除验货磅单失败,没有相关验货磅单数据"),
    DINAS_INSPECTION_GOODS_DELETED_FAILED_HAS_ALREADY(0,"删除验货磅单失败，有已结算过的数据,不能删除已结算过的数据"),
    DINAS_INSPECTION_GOODS_UPDATE_FAILED_HAS_ALREADY(0,"编辑验货磅单失败，有已结算过的数据,不能编辑已结算过的数据"),
    DINAS_INSPECTION_GOODS_DELETED_PURCHASE_FAILED(0,"删除验货磅单,删除采购结算单失败"),
    DINAS_INSPECTION_GOODS_DELETED_SALE_FAILED(0,"删除验货磅单,删除销售结算单失败"),
    DINAS_INSPECTION_GOODS_SAVE_FAILED_DELETE_OLD_PURCHASE_FAILED(0,"保存验货磅单失败,删除旧的采购结算失败"),
    DINAS_INSPECTION_GOODS_SAVE_FAILED_DELETE_OLD_SALE_FAILED(0,"保存验货磅单失败,删除旧的销售结算失败"),
    DINAS_INSPECTION_GOODS_SAVE_PURCHASE_FAILED(0,"保存验货磅单生成采购结算失败"),
    DINAS_INSPECTION_GOODS_SAVE_SALE_FAILED(0,"保存验货磅单生成销售结算失败"),



    /**
     * 样品
     */
    SMAPLE_UPDATE_FAILED(0, "样品更新失败"),

    /**
     * 合同
     */
    CONTRACT_SAVE_FAILED(0, "合同新增失败"),
    CONTRACT_NUMBER_EXISTED(0, "合同编号已存在，请勿重复"),
    CONTRACT_ATTACHMENT_ADD_FAILED(0, "合同附件新增失败"),
    CONTRACT_PAY_FAILED(0, "合同付款失败"),
    CONTRACT_SETTLEMENT_NOT_FOUND(0, "合同结算情况查询失败"),
    CONTRACT_SETTLEMENT_CONFIRMED(0, "合同已结算，无法操作"),
    CONTRACT_SETTLEMENT_NOT_FOUND_OR_CONFIRMED(0, "合同已完成或该结算单已确认"),
    CONTRACT_SETTLEMENT_AMOUNT_SAVE_FAILED(0, "合同结算数量修改失败"),
    CONTRACT_SETTLEMENT_CONFIRM_FAILED(0, "合同结算确认失败"),
    CONTRACT_COMPLETED(0, "合同已完成，无法操作"),
    CONTRACT_NO_PAY_RECORD(0, "付款未完成，请付款"),
    CONTRACT_NO_BATCH_RECORD(0, "无批次信息，无法完成合同"),
    CONTRACT_COMPLETE_FAILED(0, "完成合同失败"),
    CONTRACT_NO_BATCH(0, "无批次信息，无法完成合同"),

    /**
     * 打印机
     */
    PRINTER_URL_NOT_FOUND(0, "打印机URL配置查询失败"),
    PRINTER_URL_NOT_TOKEN(0, "当前称重设备暂未绑定打印机，请稍后重试"),
    PRINTER_SYSTEM_ERROR(0, "打印机异常，请稍后重试"),
    PRINTER_TYPE_ERROR(0, "打印机业务类型参数有误"),


    /**
     * 物流相关
     */
    CONTRACT_BUSINESS_ID_IS_EMPTY(0, "合同业务id为空，请确认！"),
    CONTRACT_BUSINESS_ID_CAN_FIND_DATA(0, "通过合同业务id查询不到合同数据！"),
    BATCH_ID_CAN_FIND_DATA(0, "通过批次业务id查询不到批次数据！"),
    TRANSPORT_SECTION_ID_CAN_FIND_DATA(0, "通过批次运输段业务id查询不到批次运输段数据！"),
    CARRIER_TRANSPORT_ID_CAN_FIND_DATA(0, "通过承运方业务id查询不到承运方数据！"),
    CARRIER_TRANSPORT_DETAIL_ID_CAN_FIND_DATA(0, "通过承运方车次id查询不到车次数据！"),
    CARRIER_TRANSPORT_DETAIL_HAS_BEEN_WEIGHT(0, "承运方车次已经进行了称重，不能进行修改！"),
    TRANSPORT_SECTION_ID_HAS_EXIST(0, "物流批次运输段的id不为空，已经存在，不能再次保存！"),
    SAVE_TRANSPORT_SECTION_FAILED(0, "保存物流批次运输段信息失败！"),
    CARRIER_TRANSPORT_ID_HAS_EXIST(0, "物流批次运输段承运商的id不为空，已经存在，不能再次保存！"),
    SAVE_CARRIER_TRANSPORT_FAILED(0, "保存物流批次运输段承运商信息失败！"),
    SAVE_CARRIER_TRANSPORT_DETAIL_FAILED(0, "保存物流批次运输段承运商车次信息失败！"),


    /**
     * 工厂系统配置相关 ===============================================
     *
     */
    AMMETER_SAVE_FAILED(0,"保存电表信息失败"),
    AMMETER_SAVE_FAILED_CODE_RE(0,"保存电表信息失败,电表编号重复"),
    AMMETER_UPDATE_FAILED(0,"修改电表信息失败"),
    AMMETER_UPDATE_FAILED_NO_DATA(0,"修改电表信息失败,没有此电表"),
    AMMETER_UPDATE_FAILED_CODE_RE(0,"修改电表信息失败,电表编号重复"),
    AMMETER_DELETED_FAILED(0,"删除电表信息失败"),

    ELECTRICITY_PRICE_SAVE_FAILED(0,"保存电价信息失败"),
    ELECTRICITY_PRICE_SAVE_FAILED_TIME_RE(0,"保存电价信息失败,电价区间重复"),
    ELECTRICITY_PRICE_SAVE_FAILED_START_TIME_BIG(0,"保存电价信息失败,电价起始日期时间大于等于结束日期时间"),
    ELECTRICITY_PRICE_UPDATE_FAILED_START_TIME_BIG(0,"修改电价信息失败,电价起始日期时间大于结束日期时间"),
    ELECTRICITY_PRICE_UPDATE_FAILED(0,"修改电价信息失败"),
    ELECTRICITY_PRICE_UPDATE_FAILED_NO_DATA(0,"修改电价信息失败,没有此电价数据"),
    ELECTRICITY_PRICE_UPDATE_FAILED_TIME_RE(0,"修改电价信息失败,电价区间重复"),
    ELECTRICITY_PRICE_DELETED_FAILED(0,"删除电价信息失败"),

    DEVICE_INSPECTION_SAVE_FAILED(0,"保存设备巡检信息失败"),
    DEVICE_INSPECTION_SAVE_FAILED_CODE_RE(0,"保存设备巡检信息失败,设备巡检编号重复"),
    DEVICE_INSPECTION_UPDATE_FAILED(0,"修改设备巡检信息失败"),
    DEVICE_INSPECTION_UPDATE_FAILED_NO_DATA(0,"修改设备巡检信息失败,没有此设备巡检数据"),
    DEVICE_INSPECTION_UPDATE_FAILED_CODE_RE(0,"修改设备巡检信息失败,设备巡检编号重复"),
    DEVICE_INSPECTION_UPDATE_FAILED_USED(0,"修改设备巡检信息失败,设备已在使用禁止修改"),
    DEVICE_INSPECTION_DELETED_FAILED_USED(0,"修改设备巡检信息失败,设备已在使用禁止删除"),
    DEVICE_INSPECTION_DELETED_FAILED(0,"删除设备巡检信息失败"),


    DEVICE_SAVE_FAILED(0,"保存设备信息失败"),
    DEVICE_SAVE_FAILED_NAME_OR_TYPE_RE(0,"保存设备信息失败,设备重复"),
    DEVICE_UPDATE_FAILED(0,"修改设备信息失败"),
    DEVICE_UPDATE_FAILED_USED(0,"修改设备信息失败,设备已在使用禁止修改"),
    DEVICE_UPDATE_FAILED_NO_DATA(0,"修改设备信息失败,没有此设备数据"),
    DEVICE_UPDATE_FAILED_NAME_OR_TYPE_RE(0,"修改设备信息失败,设备重复"),
    DEVICE_DELETED_FAILED(0,"删除设备信息失败"),
    DEVICE_DELETED_FAILED_USED(0,"删除设备信息失败,设备已在使用禁止删除"),

    WEIGH_DEVICE_SAVE_FAILED(0,"保存称重设备信息失败"),
    WEIGH_DEVICE_SAVE_FAILED_NAME_OR_CODE_RE(0,"保存称重设备信息失败,设备重复"),
    WEIGH_DEVICE_UPDATE_FAILED(0,"修改称重设备信息失败"),
    WEIGH_DEVICE_UPDATE_FAILED_NO_DATA(0,"修改称重设备信息失败,没有设备数据"),
    WEIGH_DEVICE_UPDATE_FAILED_USED(0,"修改称重设备信息失败,称重设备已在使用禁止修改"),
    WEIGH_DEVICE_UPDATE_FAILED_NAME_OR_CODE_RE(0,"修改称重设备信息失败,设备重复"),
    WEIGH_DEVICE_DELETED_FAILED(0,"删除称重设备信息失败"),

    PLC_DEVICE_SAVE_FAILED(0,"保存PLC设备信息失败"),
    PLC_DEVICE_SAVE_FAILED_NAME_OR_TYPE_RE(0,"保存PLC设备信息失败,设备重复"),
    PLC_DEVICE_UPDATE_FAILED(0,"修改PLC设备信息失败"),
    PLC_DEVICE_UPDATE_FAILED_NO_DATA(0,"修改PLC设备信息失败,没有设备数据"),
    PLC_DEVICE_UPDATE_FAILED_NAME_OR_TYPE_RE(0,"修改PLC设备信息失败,设备重复"),
    PLC_DEVICE_DELETED_FAILED(0,"删除PLC设备信息失败"),


    REPOSITORY_SAVE_FAILED(0,"保存仓库信息失败"),
    REPOSITORY_SAVE_FAILED_NAME_RE(0,"保存仓库信息失败,名称重复"),
    REPOSITORY_UPDATE_FAILED(0,"修改仓库信息失败"),
    REPOSITORY_UPDATE_FAILED_USED(0,"修改仓库信息失败,仓库正在使用禁止修改"),
    REPOSITORY_UPDATE_FAILED_NO_DATA(0,"修改仓库信息失败,没有仓库数据"),
    REPOSITORY_UPDATE_FAILED_NAME_RE(0,"修改仓库信息失败,名称重复"),
    REPOSITORY_DELETED_FAILED(0,"删除仓库信息失败"),
    REPOSITORY_DELETED_FAILED_USED(0,"删除仓库信息失败,仓库正在使用禁止删除"),


    MATERIALS_CONSUMPTION_SAVE_FAILED(0,"保存原料吨耗信息失败"),
    MATERIALS_CONSUMPTION_SAVE_FAILED_NAME_RE(0,"保存原料吨耗信息失败,产品重复"),
    MATERIALS_CONSUMPTION_UPDATE_FAILED(0,"修改原料吨耗信息失败"),
    MATERIALS_CONSUMPTION_UPDATE_FAILED_NAME_RE(0,"修改原料吨耗信息失败,产品名称重复"),
    MATERIALS_CONSUMPTION_DELETED_FAILED(0,"删除原料吨耗信息失败"),

    PRODUCT_CATEGORY_SAVE_FAILED(0,"保存产品分类信息失败"),
    PRODUCT_CATEGORY_SAVE_FAILED_NAME_RE(0,"保存产品分类信息失败,名称重复"),
    PRODUCT_CATEGORY_UPDATE_FAILED(0,"修改产品分类信息失败"),
    PRODUCT_CATEGORY_UPDATE_FAILED_NO_DATA(0,"修改产品分类信息失败，数据不存在"),
    PRODUCT_CATEGORY_UPDATE_FAILED_NAME_RE(0,"修改产品分类信息失败,名称重复"),
    PRODUCT_CATEGORY_UPDATE_FAILED_PROHIBIT_UPDATE_DEFAULT(0,"修改产品分类信息失败,id参数异常,不能修改默认分类"),
    PRODUCT_CATEGORY_DELETED_FAILED(0,"删除产品分类信息失败"),
    PRODUCT_CATEGORY_DELETED_FAILED_PROHIBIT_DELETE_DEFAULT(0,"删除产品分类信息失败,id参数异常,不能删除默认分类"),


    PRODUCT_SAVE_FAILED(0,"保存产品信息失败"),
    PRODUCT_SAVE_TEST_IN_FAILED(0,"保存产品化验输入项信息失败"),
    PRODUCT_SAVE_TEST_OUT_FAILED(0,"保存产品化验结果项信息失败"),
    PRODUCT_SAVE_SETTLEMENT_ATTRIBUTE_FAILED(0,"保存产品结算属性信息失败"),
    PRODUCT_SAVE_FAILED_NAME_RE(0,"保存产品信息失败,产品名称重复"),
    PRODUCT_UPDATE_FAILED(0,"修改产品信息失败"),
    PRODUCT_UPDATE_SPEC_FAILED(0,"修改产品规格失败"),
    PRODUCT_UPDATE_SPEC_FAILED_SIZE_TOO_BIG(0,"修改产品规格失败,合格线数量不能大于1"),
    PRODUCT_UPDATE_SPEC_FAILED_SORT_RE(0,"修改产品规格失败,排序重复"),
    PRODUCT_UPDATE_FAILED_NO_DATA(0,"修改产品信息失败,没有此条记录"),
    PRODUCT_UPDATE_TEST_FAILED_NO_DATA(0,"修改产品化验配置信息失败,没有此产品信息"),
    PRODUCT_UPDATE_SPEC_FAILED_NO_DATA(0,"修改产品化验规格信息失败,没有此产品信息"),
    PRODUCT_UPDATE_FAILED_USED(0,"修改产品信息失败,此产品正在使用禁止修改"),
    PRODUCT_UPDATE_FAILED_NAME_RE(0,"修改产品信息失败,产品名称重复"),
    PRODUCT_DELETED_FAILED(0,"删除产品信息失败"),
    PRODUCT_DELETED_FAILED_USED(0,"删除产品信息失败,此产品正在使用禁止删除"),
    PRODUCT_DELETED_TEST_IN_FAILED(0,"删除旧的产品化验输入项失败"),
    PRODUCT_DELETED_TEST_OUT_FAILED(0,"删除旧的产品化验输出项失败"),
    PRODUCT_DELETED_SETTLEMENT_ATTRIBUTE_FAILED(0,"删除旧的产品结算属性失败"),


    OPENING_INVENTORY_SAVE_FAILED(0,"保存期初库存失败"),
    OPENING_INVENTORY_SAVE_FAILED_CODE_RE(0,"保存期初库存失败,编号重复"),
    OPENING_INVENTORY_UPDATE_FAILED(0,"修改期初库存信息失败"),
    OPENING_INVENTORY_UPDATE_FAILED_CODE_RE(0,"修改期初库存信息失败,编号重复"),
    OPENING_INVENTORY_DELETED_FAILED(0,"删除期初库存失败"),

    OPENING_INVENTORY_DETAIL_SAVE_FAILED(0,"保存期初库存明细失败"),


    CUSTOMER_SAVE_FAILED(0,"保存客户信息失败"),
    CUSTOMER_CREAT_ENTERPRISE_SAVE_FAILED(0,"保存客户信息,生成企业信息失败"),
    CUSTOMER_SAVE_FAILED_NAME_RE(0,"保存客户信息失败,客户名称重复"),
    CUSTOMER_UPDATE_FAILED(0,"修改客户信息失败"),
    CUSTOMER_UPDATE_FAILED_NO_DATA(0,"修改客户信息失败,没有此客户"),
    CUSTOMER_UPDATE_FAILED_NAME_RE(0,"修改客户信息失败,客户重复"),
    CUSTOMER_DELETED_FAILED(0,"删除客户信息失败"),

    SUPPLIER_SAVE_FAILED(0,"保存供应商信息失败"),
    SUPPLIER_SAVE_FAILED_CARRIER_C_EMPTY(0,"承运商类别不能为空"),
    SUPPLIER_UPDATE_FAILED_CARRIER_C_EMPTY(0,"承运商类别不能为空"),
    SUPPLIER_CREAT_ENTERPRISE_SAVE_FAILED(0,"保存供应商,生成企业信息信息失败"),
    SUPPLIER_SAVE_FAILED_NAME_RE(0,"保存供应商信息失败,供应商重复"),
    SUPPLIER_UPDATE_FAILED(0,"修改供应商信息失败"),
    SUPPLIER_UPDATE_FAILED_NO_DATA(0,"修改供应商信息失败,没有此供应商"),
    SUPPLIER_UPDATE_FAILED_CODE_RE(0,"修改供应商信息失败,供应商重复"),
    SUPPLIER_DELETED_FAILED(0,"删除供应商信息失败"),

    CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED(0,"保存客户或供应商账号失败"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED_NO_CS_DATA(0,"保存客户或供应商账号失败,没有客户或供应商信息"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED_NO_E_DATA(0,"保存客户或供应商账号失败,没企业信息"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_CREAT_ENTERPRISE_SAVE_FAILED(0,"保存客户或供应商账号,生成用户失败"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_SAVE_FAILED_ACCOUNT_RE(0,"保存客户或供应商账号失败,客户或供应商账号重复"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_UPDATE_FAILED(0,"修改客户或供应商账号失败"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_UPDATE_PASSWORD_FAILED_NO_DATA(0,"修改客户或供应商账号密码失败，没有此客户或供应商账号信息"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_UPDATE_PASSWORD_FAILED_NO_USER_DATA(0,"修改客户或供应商账号密码失败，没有用户账号信息"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_UPDATE_FAILED_NO_DATA(0,"修改客户或供应商账号失败,没有此客户或供应商账号"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_UPDATE_FAILED_NAME_RE(0,"修改客户或供应商账号失败,客户或供应商账号重复"),
    CUSTOMER_OR_SUPPLIER_ACCOUNT_DELETED_FAILED(0,"删除客户或供应商账号信息失败"),


    TEST_ATTRIBUTE_SAVE_FAILED(0,"保存化验属性信息失败"),
    TEST_ATTRIBUTE_SAVE_FAILED_NAME_OR_TYPE_RE(0,"保存化验属性信息失败,化验属性重复"),
    TEST_ATTRIBUTE_UPDATE_FAILED(0,"修改化验属性信息失败"),
    TEST_ATTRIBUTE_UPDATE_FAILED_USED(0,"修改化验属性信息失败,化验属性已在使用禁止修改"),
    TEST_ATTRIBUTE_UPDATE_FAILED_NO_DATA(0,"修改化验属性信息失败,没有此化验属性数据"),
    TEST_ATTRIBUTE_UPDATE_FAILED_NAME_OR_TYPE_RE(0,"修改化验属性信息失败,化验属性重复"),
    TEST_ATTRIBUTE_DELETED_FAILED(0,"删除化验属性信息失败"),
    TEST_ATTRIBUTE_DELETED_FAILED_USED(0,"删除化验属性信息失败,化验属性已在使用禁止删除"),


    LOCATION_SAVE_FAILED(0,"保存物流地点信息失败"),
    LOCATION_SAVE_FAILED_NAME_RE(0,"保存物流地点信息失败,物流地点重复"),
    LOCATION_UPDATE_FAILED(0,"修改物流地点信息失败"),
    LOCATION_UPDATE_FAILED_USED(0,"修改物流地点信息失败,物流地点已在使用禁止修改"),
    LOCATION_UPDATE_FAILED_NO_DATA(0,"修改物流地点信息失败,没有此物流地点数据"),
    LOCATION_UPDATE_FAILED_NAME_RE(0,"修改物流地点信息失败,物流地点重复"),
    LOCATION_DELETED_FAILED(0,"删除物流地点信息失败"),
    LOCATION_DELETED_FAILED_USED(0,"删除物流地点信息失败,物流地点已在使用禁止删除"),


    RAW_MATERIAL_LOSS_SAVE_FAILED(0,"保存原料损耗信息失败"),
    RAW_MATERIAL_LOSS_SAVE_FAILED_NAME_RE(0,"保存原料损耗信息失败,产品重复"),
    RAW_MATERIAL_LOSS_UPDATE_FAILED(0,"修改原料损耗信息失败"),
    RAW_MATERIAL_LOSS_UPDATE_FAILED_USED(0,"修改原料损耗信息失败,原料损耗已在使用禁止修改"),
    RAW_MATERIAL_LOSS_UPDATE_FAILED_NO_DATA(0,"修改原料损耗信息失败,没有此原料损耗数据"),
    RAW_MATERIAL_LOSS_UPDATE_FAILED_NAME_RE(0,"修改原料损耗信息失败,产品重复"),
    RAW_MATERIAL_LOSS_DELETED_FAILED(0,"删除原料损耗信息失败"),
    RAW_MATERIAL_LOSS_DELETED_FAILED_USED(0,"删除原料损耗信息失败,原料损耗已在使用禁止删除"),



    LOOK_BOARD_BI_SAVE_FAILED(0,"保存看板BI配置信息失败"),
    LOOK_BOARD_BI_GET_FAILED(0,"查询启用的看板BI配置信息失败,请求参数错误"),
    LOOK_BOARD_BI_SAVE_FAILED_NAME_RE(0,"保存看板BI配置信息失败,看板BI配置重复"),
    LOOK_BOARD_BI_UPDATE_FAILED(0,"修改看板BI配置信息失败"),
    LOOK_BOARD_BI_UPDATE_FAILED_USED(0,"修改看板BI配置信息失败,看板BI配置已在使用禁止修改"),
    LOOK_BOARD_BI_UPDATE_FAILED_NO_DATA(0,"修改看板BI配置信息失败,没有此看板BI配置数据"),
    LOOK_BOARD_BI_UPDATE_FAILED_NAME_RE(0,"修改看板BI配置信息失败,看板BI配置重复"),
    LOOK_BOARD_BI_DELETED_FAILED(0,"删除看板BI配置信息失败"),
    LOOK_BOARD_BI_ENABLE_FAILED(0,"启用看板BI配置信息失败"),
    LOOK_BOARD_BI_DELETED_FAILED_USED(0,"删除看板BI配置信息失败,看板BI配置已在使用禁止删除"),


    REPORT_FORMS_SAVE_FAILED(0,"保存报表配置信息失败"),
    REPORT_FORMS_GET_FAILED(0,"查询启用的报表配置信息失败，请求参数错误"),
    REPORT_FORMS_STOP_FAILED(0,"停用报表配置信息失败"),
    REPORT_FORMS_SAVE_FAILED_NAME_RE(0,"保存报表配置信息失败,报表配置重复"),
    REPORT_FORMS_UPDATE_FAILED(0,"修改报表配置信息失败"),
    REPORT_FORMS_UPDATE_FAILED_USED(0,"修改报表配置信息失败,报表配置已在使用禁止修改"),
    REPORT_FORMS_UPDATE_FAILED_NO_DATA(0,"修改报表配置信息失败,没有此报表配置数据"),
    REPORT_FORMS_UPDATE_FAILED_NAME_RE(0,"修改报表配置信息失败,报表配置重复"),
    REPORT_FORMS_DELETED_FAILED(0,"删除报表配置信息失败"),
    REPORT_FORMS_ENABLE_FAILED(0,"启用报表配置信息失败"),
    REPORT_FORMS_DELETED_FAILED_USED(0,"删除报表配置信息失败,报表配置已在使用禁止删除"),


    /**
     * 导出excel
     */
    EXPORT_EXCEL_ERROR(0, "导出excel出错"),

    /**
     * 库存相关
     */
    PRODUCT_STORAGE_NOT_EXIST(0, "库存中不存在相关产品"),
    STORAGE_REPEATED_SUBMISSION(0,"该条数据已经入库，请勿重复提交！"),


    /**
     * 报表相关
     */
    PRODUCT_SPEC_NOT_EXIST(1, "该产品没有合格线！"),
    FURNACE_NOT_EXIST(0, "根据炉号未查询到矿热炉！"),
    /**
     * 样品相关
     */
    FORMULA_ERROR(0, "计算错误,请检查公式是否正确"),

    ;

    private Integer code;
    private String message;
}
