package com.bee.platform.common.utils;

/**
 * notes 对象工具类
 * author liang.li
 * create 2019/9/30
 **/
public class IntegerUtils {
    final static int[] sizeTable = {9, 99, 999, 9999, 99999, 999999, 9999999, 99999999, 999999999, Integer.MAX_VALUE};

    public static int sizeOfInt(int x) {
        for (int i = 0; ; i++) {
            if (x <= sizeTable[i]) {
                return i + 1;
            }
        }
    }

    public static String getDecorateInteger(int length, Integer source) {
        int i = sizeOfInt(source);
        if (length > i) {
            int i1 = length - i;
            StringBuilder sb = new StringBuilder();
            for (; i1 > 0; i1--) {
                sb.append(0);
            }
            sb.append(source);
            return sb.toString();
        }
        return source.toString();
    }

}
