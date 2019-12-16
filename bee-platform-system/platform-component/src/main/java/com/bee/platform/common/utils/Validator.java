package com.bee.platform.common.utils;

import org.apache.commons.lang.StringUtils;

import java.util.regex.Pattern;

/**
 * Created by CrazyMouse on 2016/11/3.
 */
@SuppressWarnings("ALL")
public class Validator {
    /**
     * 正则表达式：验证用户名
     */
    public static final String REGEX_USERNAME = "^[a-zA-Z]\\w{5,17}$";

    /**
     * 正则表达式：验证密码
     */
    public static final String REGEX_PASSWORD = "^(?=.*[a-zA-Z])(?=.*[0-9])[a-zA-Z0-9]{6,16}$";

    /**
     * 正则表达式：验证手机号
     */
    public static final String REGEX_MOBILE = "^(1)\\d{10}$";

    /**
     * 正则表达式：验证邮箱
     */
    public static final String REGEX_EMAIL = "^([a-z0-9A-Z]+[-|\\.]?)+[a-z0-9A-Z]@([a-z0-9A-Z]+(-[a-z0-9A-Z]+)?\\.)+[a-zA-Z]{2,}$";

    /**
     * 正则表达式：验证身份证
     */
    public static final String REGEX_ID_CARD = "(^\\d{18}$)|(^\\d{15}$)";

    /**
     * 正则表达式：验证入库单号
     */
    public static final String REGEX_STORAGE_CODE = "(^[A-Z]{2}\\d{11}$)";

    /**
     * 校验用户名
     *
     * @param username
     * @return 校验通过返回true，否则返回false
     */
    public static boolean isUsername(String username) {
        return Pattern.matches(REGEX_USERNAME, username);
    }

    /**
     * 校验密码
     *
     * @param password
     * @return 校验通过返回true，否则返回false
     */
    public static boolean isPassword(String password) {
        if(StringUtils.isBlank(password)){
            return false;
        }
        return Pattern.matches(REGEX_PASSWORD, password);
    }

    /**
     * 校验手机号
     *
     * @param mobile
     * @return 校验通过返回true，否则返回false
     */
    public static boolean isMobile(String mobile) {
        if(StringUtils.isBlank(mobile)){
            return false;
        }
        return Pattern.matches(REGEX_MOBILE, mobile);
    }

    /**
     * 校验邮箱
     *
     * @param email
     * @return 校验通过返回true，否则返回false
     */
    public static boolean isEmail(String email) {
        if(StringUtils.isBlank(email)){
            return false;
        }
        return Pattern.matches(REGEX_EMAIL, email);
    }

    /**
     * 校验身份证
     *
     * @param idCard
     * @return 校验通过返回true，否则返回false
     */
    public static boolean isIDCard(String idCard) {
        return Pattern.matches(REGEX_ID_CARD, idCard);
    }

    /**
     * 判断字段是否为空
     * @param params
     * @return
     */
    @SuppressWarnings("AliEqualsAvoidNull")
    public static boolean isNull(Object... params) {
        for (int i = 0; i < params.length; i++) {
            if (("").equals(params[i])) {
                return true;
            }
        }
        return false;
    }

    /**
     * @Description 校验Erp采购收货入库订单
     * @Param code
     * @Date 2019/5/30 11:06
     * @Author xin.huang
     * @Return
     */
    public static boolean validateCode(String code) {
        if(StringUtils.isBlank(code)){
            return false;
        }
        return Pattern.matches(REGEX_STORAGE_CODE, code);
    }

}
