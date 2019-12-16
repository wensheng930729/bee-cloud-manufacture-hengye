package com.bee.platform.common.utils;


import org.apache.commons.lang.StringUtils;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;

/**
 * @ClassName: LocalDateUtils
 * @Description: 时间处理工具类
 * @Author: fei.sun
 * @Date: 2019/9/24 11:38
 * @Version: 1.0
 */
public class LocalDateUtils {

    public static final String Y_M_D = "yyyy-MM-dd";
    public static final String Y_M_D_H_M_S = "yyyy-MM-dd HH:mm:ss";

    public static LocalDateTime parseDate(String date,String pattern){
        if(StringUtils.isBlank(pattern)){
            pattern = "yyyy-MM-dd HH:mm:ss";
        }
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(pattern);
        if(StringUtils.isBlank(date)){
            return null;
        }
        return LocalDateTime.parse(date, dtf);
    }

    public static LocalDate parseDateToLocalDate(String date, String pattern){
        if(StringUtils.isBlank(pattern)){
            pattern = "yyyy-MM-dd";
        }
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(pattern);
        if(StringUtils.isBlank(date)){
            return null;
        }
        return LocalDate.parse(date, dtf);
    }

    public static String formatDate(TemporalAccessor temporal, String pattern){
        if(StringUtils.isBlank(pattern)){
            pattern = "yyyy-MM-dd HH:mm:ss";
        }
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(pattern);
        if(temporal == null){
            return null;
        }
        return dtf.format(temporal);
    }

    /**
     * 获取今天的一周之前的日期
     */
    public static String getLastWeekDay(String pattern){
        if(StringUtils.isBlank(pattern)){
            pattern = Y_M_D;
        }
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(pattern);
        LocalDateTime lastWeek = LocalDateTime.now().minusWeeks(1).plusDays(1);
        return lastWeek.format(dtf);
    }

    /**
     * 获取今天的一月之前的日期
     */
    public static String getLastMonthDay(String pattern){
        if(StringUtils.isBlank(pattern)){
            pattern = Y_M_D;
        }
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(pattern);
        LocalDateTime lastMonth = LocalDateTime.now().minusMonths(1).plusDays(1);
        return lastMonth.format(dtf);
    }

    /**
     * 获取今天的一年之前的日期
     */
    public static String getLastYearDate(String pattern){
        if(StringUtils.isBlank(pattern)){
            pattern = Y_M_D;
        }
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(pattern);
        LocalDateTime lastYear = LocalDateTime.now().minusYears(1).plusDays(1);
        return lastYear.format(dtf);
    }

    public static void main(String[] args) {
        LocalDateTime localDateTime = LocalDateUtils.parseDate("2019年09月13日 16:23:50", "yyyy年MM月dd日 HH:mm:ss");
        System.out.println("str to date :"+localDateTime);
        String s = LocalDateUtils.formatDate(LocalDateTime.now(), "yyyy年MM月dd日 HH:mm:ss");
        System.out.println("date to str : "+s);
    }
}
