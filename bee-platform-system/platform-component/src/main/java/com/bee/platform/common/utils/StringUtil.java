package com.bee.platform.common.utils;

import org.apache.commons.lang3.StringUtils;

/**
 * @Classname StringUtil
 * @Description 字符串处理工具类
 * @Date 2019/5/7 16:22
 * @Author xin.huang
 */
public class StringUtil {

    private static final String DEFAULT = "-";

    private StringUtil() {
    }

    /**
     * @Description 截取邮箱显示
     * @Param email
     * @Author xin.huang
     * @Date 17:09 2019/5/7
     */
    public static String getSpliceEmail(String email) {
        if (StringUtils.isBlank(email)) {
            return "";
        }
        String start = email.substring(0, email.indexOf("@"));
        String end = email.substring(email.indexOf("@"));
        String identifier = "***";
        if (start.length() > 2) {
            start = start.substring(0, 2);
        }
        StringBuffer str = new StringBuffer();
        str.append(start).append(identifier).append(end);
        return str.toString();
    }

    /**
     * @Description 按指定长度截取字符串
     * @Param length
     * @Return
     * @Date 2019/5/13 15:05
     * @Author xin.huang
     */
    public static String splice(String str, int length) {
        if (StringUtils.isBlank(str)) {
            return "";
        }
        if (str.length() > length) {
            return str.substring(0, length);
        }
        return str;
    }

    /**
     * 将阿拉伯数字转换成中文小写汉字
     * @param num
     * @return
     */
    public static String changeNum(int num) {
        StringBuilder result = new StringBuilder();
        String[] gewei = {"一","二","三","四","五","六","七","八","九"};
        String[] shiwei = {"十","百","千","万","十万","百万"};
        String a = String.valueOf(num);
        char[] char_a = a.toCharArray();
        if("0".equals(char_a[char_a.length-1]) && char_a.length == 1){
            result.append("零");
        }else{
            for(int i=0; i<char_a.length; i++){
                int flag = Integer.parseInt(String.valueOf(char_a[i]));
                if((char_a.length-i-2) >= 0 ){
                    if(flag == 0 && i == (char_a.length-1)){
                        result.append("十");
                    }else if(flag == 0){
                        result.append("零");
                    }else {
                        result.append(gewei[flag-1] + shiwei[char_a.length-i-2]);
                    }
                }else {
                    if(flag == 0 && i == (char_a.length-1)){
                    }else {
                        result.append(gewei[flag-1]);
                    }
                }
            }
        }
        return result.toString();
    }

    /**
     * @Description 按指定起始点截取字符串
     * @Param length
     * @Return
     * @Date 2019/5/13 15:05
     * @Author xin.huang
     */
    public static String spliceStr(String str, int startIndex) {
        if (StringUtils.isBlank(str)) {
            return "";
        }
        if (str.length() > startIndex) {
            return str.substring(startIndex);
        }
        return str;
    }

}
