package com.bee.platform.cloud.si.manufacture.socket;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description Socket解析工具类
 * @Date 2019/9/25 17:17
 */
public class SocketUtils {

    /**
     * @Description 解析16进制数据
     * @author chenxm66777123
     * @Date 2019/9/26 13:45
     * @version 1.0.0
     */
    public static String convertHexToString(String hex) {
        StringBuilder sb = new StringBuilder();
        StringBuilder temp = new StringBuilder();
        for (int i = 0; i < hex.length() - 1; i += 2) {

            // grab the hex in pairs
            String output = hex.substring(i, (i + 2));
            // convert hex to decimal
            int decimal = Integer.parseInt(output, 16);
            // convert the decimal to character
            sb.append((char) decimal);

            temp.append(decimal);
        }
        return sb.toString();
    }

    /**
     * @Description 将字符串倒序排列
     * @author chenxm66777123
     * @Date 2019/9/26 13:45
     * @version 1.0.0
     */
    public static String reverseString(String str) {
        StringBuffer buffer = new StringBuffer(str);
        return buffer.reverse().toString();
    }
}
