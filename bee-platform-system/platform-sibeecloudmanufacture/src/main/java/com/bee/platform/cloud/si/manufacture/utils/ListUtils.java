package com.bee.platform.cloud.si.manufacture.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @Author zhanghu
 * @Description: 判断list集合是否为空
 * @Date: Create in 10:18 2018/11/1
 * @modified by:
 */
public class ListUtils {
    private static final Logger log = LoggerFactory.getLogger(ListUtils.class);

    /**
     * 判断list集合是否为空，如果为空，则返回true，否则返回false
     * @param list 传入的集合
     * @return boolean
     */
    public static boolean isEmpty(List list) {
        return list == null || list.isEmpty();
    }

    /**
     * 判断list集合是否为空，如果为空，则返回false，否则返回true
     * @param list 传入的list
     * @return boolean
     */
    public static boolean isNotEmpty(List list) {
        return list != null && !list.isEmpty();
    }

    public static void main(String[] args) {
        log.info("fdsfsd");
        log.debug("aaaaaa");
    }
}
