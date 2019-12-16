package com.bee.platform.common.utils;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * @ClassName ResourcesUtil
 * @Description 资源文件读取工具类
 * @Author xin.huang
 * @Date 2019/5/5 16:08
 * @Version 1.0.0
 */
public class ResourcesUtil {
    private static ResourceBundle resourceBundle;

    private ResourcesUtil() {
    }

    static {
        resourceBundle = ResourceBundle.getBundle("messages");
    }

    /**
     * 通过key从资源文件读取内容，并格式化
     *
     * @param key
     *
     * @return
     */
    public static String getValue(int key) {
        String retValue = "";
        try {
            retValue = String.valueOf(resourceBundle.getObject(key + ""));
        } catch (MissingResourceException e) {
            return retValue;
        }
        return retValue;
    }

}
