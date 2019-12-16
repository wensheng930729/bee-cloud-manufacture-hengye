package com.bee.platform.cloud.si.manufacture.utils;

import java.security.MessageDigest;

/**
 * md5加密
 *
 * @author
 * @date
 */
public class MD5 {

    public static String encrypt(String dataStr) {
        try {
            MessageDigest m = MessageDigest.getInstance("MD5");
            m.update(dataStr.getBytes("UTF8"));
            byte s[] = m.digest();
            String result = "";
            for (int i = 0; i < s.length; i++) {
                result += Integer.toHexString((0x000000FF & s[i]) | 0xFFFFFF00).substring(6);
            }
            return result.toUpperCase();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return "";
    }
}