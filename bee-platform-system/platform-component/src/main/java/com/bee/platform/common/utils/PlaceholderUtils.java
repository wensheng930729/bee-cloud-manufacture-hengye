package com.bee.platform.common.utils;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 配置文件或模板中的占位符替换工具类.
 * @Date 2018年12月27日
 */
@Slf4j
public class PlaceholderUtils {

    // 占位符前缀: "${"
    public static final String PLACEHOLDER_PREFIX = "${";

    // 占位符的后缀: "}"
    public static final String PLACEHOLDER_SUFFIX = "}";

    /**
     * @Description 替换占位符
     * @author chenxm66777123
     * @Date 2018年12月27日
     * @version 1.0.0
     */
    public static String resolvePlaceholders(String text, Map<String, Object> parameter) {
        //判断参数是否为空
        if (parameter == null || parameter.isEmpty()) {
            return text;
        }
        //将模板内容转到字符缓冲区
        StringBuffer buf = new StringBuffer(text);
        //获取第一个占位符前缀index
        int startIndex = buf.indexOf(PLACEHOLDER_PREFIX);
        //有占位符前缀开始循环
        while (startIndex != -1) {
            //获取最近占位符的后缀index
            int endIndex = buf.indexOf(PLACEHOLDER_SUFFIX);
            //判断是否有后缀 没有就返回 结束循环
            if (endIndex != -1) {
                //获取替换内容
                String placeholder = buf.substring(startIndex + PLACEHOLDER_PREFIX.length(), endIndex);
                try {
                    //去map中获取占位符对应的内容
                    String propVal = null;
                    if (parameter.get(placeholder) != null) {
                        propVal = String.valueOf("(" + parameter.get(placeholder) + ")");
                    }
                    //不为空替换掉占位符
                    if (propVal != null) {
                        buf.replace(startIndex, endIndex + PLACEHOLDER_SUFFIX.length(), propVal);
                    } else {
                        log.warn("模板无法替换 '" + placeholder + "' in [" + text + "] ");
                        break;
                    }
                } catch (Exception ex) {
                    log.warn("模板无法替换 '" + placeholder + "' in [" + text + "]: " + ex);
                    break;
                }
                startIndex = buf.indexOf(PLACEHOLDER_PREFIX);
            } else {
                startIndex = -1;
            }
        }
        return buf.toString();
    }

}
