package com.bee.platform.common.utils;

import org.apache.commons.lang3.StringUtils;

import java.text.MessageFormat;
import java.util.ResourceBundle;

/**
 * @ClassName MiddleNoticeUtils
 * @Description 中台系统通知资源文件读取工具类
 * @Author xin.huang
 * @Date 2019/5/5 16:08
 * @Version 1.0.0
 */
public class MiddleNoticeUtils {
    private static ResourceBundle resourceBundle;

    private MiddleNoticeUtils() {
    }

    static {
        resourceBundle = ResourceBundle.getBundle("middleNotice");
    }

    /**
     * 通过key从资源文件读取内容，并格式化
     *
     * @param key
     * @param objs
     *
     * @return 格式化后的内容
     */
    public static String getValue(int key, Object...objs) {
        String retValue = null;
        retValue = (String) resourceBundle.getObject(key + "");
        if (StringUtils.isEmpty(retValue)) {
            retValue = (String) resourceBundle.getObject("0");
        }

        if (objs != null) {
            retValue = MessageFormat.format(retValue, objs);
        }

        return retValue;
    }

//    public static void show(String[] args) {
//        String hello = getValue(19, "hello");
//        System.out.println(hello);
//    }

}
