package com.bee.platform.common.utils;

import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.time.DateFormatUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.*;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

/**
 * @author chenxm66777123
 * @description 日期操作工具类
 */
public class DateUtils {

    private static final Logger logger = LoggerFactory.getLogger(DateUtils.class);


    // 常用的格式
    public static final String Y_M_D = "yyyy-MM-dd";
    public static final String Y_M = "yyyy-MM";
    public static final String Y_M_D_H_M = "yyyy-MM-dd HH:mm";
    public static final String DEFAULT = "yyyy年MM月dd日 HH:mm";
    public static final String Y_M_D_H_M_S = "yyyy-MM-dd HH:mm:ss";
    public static final String YMD = "yyyyMMdd";
    public static final String YMDHM = "yyyyMMddHHmm";
    public static final String YMDHMS = "yyyyMMddHHmmss";
    public static final String ymd = "yyyy/MM/dd";
    public static final String ymd_H_M = "yyyy/MM/dd HH:mm";
    public static final String ymd_H_M_S = "yyyy/MM/dd HH:mm:ss";
    public static final String UTC = "yyyy-MM-dd'T'HH:mm:ss.SSS Z";
    public static final String UTC2 = "yyyy-MM-dd'T'HH:mm:ss.SSS Z";
    public static final String TIME_SUFFIX = " 00:00:00";
    public static final String TIME_END = " 23:59:59";

    /**
     * 格式化方法(只能格式化数据为【yyyy-MM-dd、yyyy-MM-dd HH:mm:ss】)
     *
     * @param format
     * @return 以字符串格式返回格式化后的日期, 也可将日期类型转化为字符串格式
     */
    public static String date2Format(String selectDate, String format, Date date) {
        if (format == null || "".equals(format)) {
            format = "yyyy-MM-dd";
        }

        SimpleDateFormat sdf = new SimpleDateFormat(format);
        String time = "";
        if (date != null) {
            time = sdf.format(date);
        } else {

            try {
                date = sdf.parse(selectDate);
                time = sdf.format(date);
            } catch (ParseException e) {
                logger.info("时间格式有误");
                return null;
            }

        }

        return time;
    }

    /**
     * 格式化方法(日期的格式化只包括【yyyy-MM-dd、yyyy-MM-dd HH:mm:ss】)
     *
     * @return 以字符串格式返回格式化后的日期
     */
    public static String format2Format(String selectDate, String dateFormat, String stringFormat) {
        if (dateFormat == null || "".equals(dateFormat)) {
            dateFormat = "yyyy-MM-dd";
        }
        if (stringFormat == null || "".equals(stringFormat)) {
            stringFormat = "yyyy-MM-dd";
        }

        SimpleDateFormat sdf = new SimpleDateFormat(dateFormat);
        Date date = null;
        String time = "";
        try {
            date = sdf.parse(selectDate);
            sdf = new SimpleDateFormat(stringFormat);
            time = sdf.format(date);
        } catch (ParseException e) {
            logger.info("时间格式有误");
            return null;
        }

        return time;
    }

    /**
     * 判断日期是否符合“YYYY-MM-DD”格式
     *
     * @param selectDate 查询日期
     * @return 是返回true，不是返回false
     */
    @SuppressWarnings("unused")
    public static boolean isValidDate(String selectDate) {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            Date date = dateFormat.parse(selectDate);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * 检查日期是否合法
     *
     * @param expDate
     * @return
     */
    @SuppressWarnings("unused")
    public static boolean checkExpDate(String expDate) {
        Date date = null;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            date = dateFormat.parse(expDate);
            int year = Integer.parseInt(expDate.substring(0, 4));
            int month = Integer.parseInt(expDate.substring(5, 7));
            int day = Integer.parseInt(expDate.substring(8, 10));
            if (month > 12 || month < 1) {
                return false;
            }

            int[] monthLengths = new int[]{0, 31, -1, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

            if (isLeapYear(year)) {
                monthLengths[2] = 29;
            } else {
                monthLengths[2] = 28;
            }

            int monthLength = monthLengths[month];
            if (day < 1 || day > monthLength) {
                return false;
            }
            return true;
        } catch (Exception e) {
            logger.info("日期格式有误！");
            return false;
        }

    }

    /**
     * 判断是否为闰年
     *
     * @param year
     * @return
     */
    private static boolean isLeapYear(int year) {
        return ((year % 4 == 0 && year % 100 != 0) || year % 400 == 0);
    }

    /**
     * 返回查询日期年份
     *
     * @param selectDate 查询日期
     * @return
     */
    public static Integer getYear(String selectDate) {
        Date date = null;
        Integer year = null;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            date = dateFormat.parse(selectDate);
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            year = cal.get(Calendar.YEAR);

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }
        return year;

    }

    /**
     * 返回查询日期的月
     *
     * @param selectDate 查询日期
     * @return
     */
    public static Integer getMonth(String selectDate) {
        Date date = null;
        Integer month = null;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            date = dateFormat.parse(selectDate);
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            month = cal.get(Calendar.MONTH) + 1;

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }
        return month;
    }

    /**
     * 返回查询日期的日
     *
     * @param selectDate 查询日期
     * @return
     */
    public static Integer getDay(String selectDate) {
        Date date = null;
        Integer day = null;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            date = dateFormat.parse(selectDate);
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            day = cal.get(Calendar.DAY_OF_MONTH);

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }
        return day;
    }

    /**
     * 返回查询日期的小时
     *
     * @param selectDate 查询日期
     * @return
     */
    public static Integer getHour(String selectDate) {
        Date date = null;
        Integer hour = null;
        SimpleDateFormat dateFormat2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        try {
            date = dateFormat2.parse(selectDate);
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            hour = cal.get(Calendar.HOUR_OF_DAY);

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }
        return hour;
    }

    /**
     * 返回查询日期的分钟
     *
     * @param selectDate 查询日期
     * @return
     */
    public static Integer getMinute(String selectDate) {
        Date date = null;
        Integer minute = null;
        SimpleDateFormat dateFormat2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        try {
            date = dateFormat2.parse(selectDate);
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            minute = cal.get(Calendar.MINUTE);

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }
        return minute;
    }

    /**
     * 返回查询日期的秒
     *
     * @param selectDate 查询日期
     * @return
     */
    public static Integer getSecond2(String selectDate) {
        Date date = null;
        Integer second = null;
        SimpleDateFormat dateFormat2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        try {
            date = dateFormat2.parse(selectDate);
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            second = cal.get(Calendar.SECOND);

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }
        return second;
    }

    /**
     * 返回查询日期的毫秒
     *
     * @param selectDate 查询日期
     * @return
     */
    public static Long getMillis(String selectDate) {
        Date date = null;
        Long millis = null;
        SimpleDateFormat dateFormat2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        try {
            date = dateFormat2.parse(selectDate);
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            millis = cal.getTimeInMillis();

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }
        return millis;
    }

    /**
     * 返回查询日期的星期 （1：星期一，2:星期二 ... 6:星期六 0:星期日）
     *
     * @param selectDate 查询日期
     * @return
     */
    public static Integer getChinaWeek(String selectDate) {
        Date date = null;
        Integer week = null;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            date = dateFormat.parse(selectDate);
            Calendar cal = Calendar.getInstance();
            cal.setTime(date);
            week = cal.get(Calendar.DAY_OF_WEEK) - 1;

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }

        return week;
    }

    /**
     * 获取当前的日期
     *
     * @return 以字符串格式返回当前日期
     */
    public static String todayDate1() {
        // 获取今日日期
        Date date = new Date();
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        // 日期格式化为字符串
        String endTime = dateFormat.format(date);

        return endTime;
    }

    /**
     * 获取当前的日期2
     *
     * @return 以字符串格式返回当前日期
     */
    public static String todayDate2() {
        // String endTime =
        // LocalDate.now().format(DateTimeFormatter.ofPattern(Y_M_D));

        // 设置时间格式
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(Y_M_D);

        // 获取今日日期
        LocalDate date = LocalDate.now();
        // 日期格式化为字符串
        String endTime = dtf.format(date);

        return endTime;
    }

    /**
     * 获取当前的日期2
     *
     * @return 获取当前日期的零时零点零分
     */
    public static String getTodayDate() {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(new Date());
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        Date date = calendar.getTime();
        SimpleDateFormat dateFormat = new SimpleDateFormat(Y_M_D_H_M_S);
        // 日期格式化为字符串
        String endTime = dateFormat.format(date);
        return endTime;
    }

    /**
     * 获得指定日期的前一天
     *
     * @param selectDate 查询时间
     * @return 返回一个格式化后的时间
     */
    public static String getSpecifiedDayBefore(String selectDate) {
        Calendar c = Calendar.getInstance();
        Date date = null;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            date = dateFormat.parse(selectDate);
        } catch (ParseException e) {
            logger.info("时间格式有误");
            return null;
        }
        c.setTime(date);
        int day = c.get(Calendar.DATE);
        c.set(Calendar.DATE, day - 1);

        String dayBefore = dateFormat.format(c.getTime());
        return dayBefore;
    }

    /**
     * 获得指定日期的后一天
     *
     * @param selectDate 查询时间
     * @return 返回一个格式化后的时间
     */
    public static String getSpecifiedDayAfter(String selectDate) {
        Calendar c = Calendar.getInstance();
        Date date = null;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            date = dateFormat.parse(selectDate);
        } catch (ParseException e) {
            logger.info("时间格式有误");
            return null;
        }
        c.setTime(date);
        int day = c.get(Calendar.DATE);
        c.set(Calendar.DATE, day + 1);

        String dayBefore = dateFormat.format(c.getTime());
        return dayBefore;
    }

    /**
     * 获取昨天的日期1
     *
     * @return 以字符串格式返回昨天日期
     */
    public static String yesterdayDate1() {
        // 获取当前日期
        Date date = new Date();
        Calendar calendar = Calendar.getInstance();
        // 获取前一天日期
        calendar.setTime(date);
        calendar.add(Calendar.DAY_OF_MONTH, -1);
        date = calendar.getTime();
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        // 日期格式化为字符串
        String endTime = dateFormat.format(date);

        return endTime;
    }

    /**
     * 获取昨天的日期2
     *
     * @return 以字符串格式返回昨天日期
     */
    public static String yesterdayDate2() {
        // String endTime =
        // LocalDate.now().minusDays(1).format(DateTimeFormatter.ofPattern("yyyy-MM-dd");

        // 设置时间格式
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern(Y_M_D);

        // 获取昨天日期
        LocalDate date = LocalDate.now().minusDays(1);
        // 日期格式化为字符串
        String endTime = dtf.format(date);

        return endTime;
    }

    /**
     * 获取查询日期前/后n天的日期
     *
     * @param selectDate
     * @param number
     * @return
     */
    public static String dateToNumber(String selectDate, int number) {
        Date date = new Date();

        String nowDay = "";
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            Calendar cal = Calendar.getInstance();

            // 字符串转换为日期格式
            date = dateFormat.parse(selectDate);
            cal.setTime(date);
            // 减去30天
            cal.add(Calendar.DAY_OF_MONTH, number);
            date = cal.getTime();
            nowDay = dateFormat.format(date);
        } catch (ParseException e) {
            logger.info("时间格式有误");
            return null;
        }
        return nowDay;
    }

    /**
     * 获得当前月份
     *
     * @return
     */
    public static String getCurrMonth() {
        Calendar cal = Calendar.getInstance();
        int year = cal.get(Calendar.YEAR);
        int month = cal.get(Calendar.MONTH) + 1;
        String months = "";
        if (month < 10) {
            months = "0" + month;
        } else {
            months = month + "";
        }
        String currentMonth = year + "-" + months;
        return currentMonth;
    }

    /**
     * 获得当前月的第一天的日期
     *
     * @return
     */
    public static String getCurrMonthFirstDay() {
        Calendar cal = Calendar.getInstance();
        cal = Calendar.getInstance();
        cal.add(Calendar.MONTH, 0);
        cal.set(Calendar.DAY_OF_MONTH, 1);
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        String firstday = dateFormat.format(cal.getTime());
        return firstday;
    }

    /**
     * 获得当前月的最后一天的日期
     *
     * @return
     */
    public static String getCurrMonthLastDay() {
        Calendar cal = Calendar.getInstance();
        cal = Calendar.getInstance();
        cal.add(Calendar.MONTH, 1);
        cal.set(Calendar.DAY_OF_MONTH, 0);
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        String lastday = dateFormat.format(cal.getTime());
        return lastday;
    }

    /**
     * 获取查询日期所在月的第一天
     *
     * @param selectDate 查询日期
     * @return
     */
    public static String getFirstDay(String selectDate) {
        Date date = null;
        String day_first = "";
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            // 字符串转日期
            date = dateFormat.parse(selectDate);
            GregorianCalendar gcLast = (GregorianCalendar) Calendar.getInstance();
            gcLast.setTime(date);
            // 获取上个月第一天
            gcLast.set(Calendar.DAY_OF_MONTH, 1);
            // 格式化日期
            day_first = dateFormat.format(gcLast.getTime());

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }

        return day_first;
    }

    /**
     * 获取查询日期所在月的最后一天
     *
     * @param selectDate 查询日期
     * @return
     */
    public static String getLastDay(String selectDate) {
        Date date = null;
        String day_last = "";
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            // 字符串转日期
            date = dateFormat.parse(selectDate);
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(date);
            // 加一个月
            calendar.add(Calendar.MONTH, 1);
            // 设置为该月第一天
            calendar.set(Calendar.DATE, 1);
            // 再减一天即为上个月最后一天
            calendar.add(Calendar.DATE, -1);
            // 格式化日期
            day_last = dateFormat.format(calendar.getTime());

        } catch (Exception e) {
            logger.info("时间格式有误");
            return null;
        }

        return day_last;
    }

    /**
     * 获取从开始时间到当前时间经历的天数(包括开始时间和结束时间)
     *
     * @param startTime 开始时间
     * @return 返回int类型数值
     */
    public static int todayBetween(String startTime) {

        // 获取今日日期
        Date date = new Date();
        // 字符串格式化为日期
        Calendar cal = Calendar.getInstance();
        long time1 = 0;
        long time2 = 0;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            cal.setTime(dateFormat.parse(startTime));
            // 获取开始时间的时间戳
            time1 = cal.getTimeInMillis();
            cal.setTime(date);
            // 获取结束时间的时间戳
            time2 = cal.getTimeInMillis();
        } catch (ParseException e) {
            logger.info("时间格式有误");
            return -1;
        }
        // 获取时间差的天数加一天
        long between_days = (time2 - time1) / (1000 * 3600 * 24) + 1;

        // 将天数转换为int返回
        return Integer.parseInt(String.valueOf(between_days));
    }

    /**
     * 获取从开始时间到结束时间经历的天数(包括开始时间和结束时间)
     *
     * @param startTime 开始时间
     * @param endTime   结束时间
     * @return 返回int类型数值
     */
    public static int daysBetween(String startTime, String endTime) {
        // 字符串格式化为日期
        Calendar cal = Calendar.getInstance();
        long time1 = 0;
        long time2 = 0;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            cal.setTime(dateFormat.parse(startTime));
            // 获取开始时间的时间戳
            time1 = cal.getTimeInMillis();
            cal.setTime(dateFormat.parse(endTime));
            // 获取结束时间的时间戳
            time2 = cal.getTimeInMillis();
        } catch (ParseException e) {
            //logger.info("时间格式有误");
            return -1;
        }
        // 获取时间差的天数加一天
        long between_days = (time2 - time1) / (1000 * 3600 * 24) + 1;

        // 将天数转换为int返回
        return Integer.parseInt(String.valueOf(between_days));
    }

    /**
     * 获取两个日期之间的所有日期(不包括结束时间)
     *
     * @param startTime 开始时间
     * @param endTime   结束时间
     * @return 返回int类型数值
     */
    public static List<String> GetDates(String startTime, String endTime) {
        Date d1 = new Date();
        Date d2 = new Date();
        List<String> dates = new ArrayList<String>();
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            Calendar cal = Calendar.getInstance();
            d1 = dateFormat.parse(startTime);
            d2 = dateFormat.parse(endTime);
            // 如果开始时间小于结束时间则执行
            while (d1.before(d2)) {
                // 循环讲数据插入到list
                dates.add(dateFormat.format(d1));
                cal.setTime(d1);
                cal.add(Calendar.DAY_OF_MONTH, 1);
                d1 = cal.getTime();
            }
        } catch (ParseException e) {
            logger.info("时间格式有误");
            return null;
        }

        return dates;
    }

    /**
     * 获取两个日期之间的所有日期(包括开始时间和结束时间)
     *
     * @param startTime 开始时间
     * @param endTime   结束时间
     * @return 返回String类型的集合
     */
    public static List<String> GetDates2(String startTime, String endTime) {
        Date d1 = new Date();
        Date d2 = new Date();
        List<String> dates = new ArrayList<String>();
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            Calendar cal = Calendar.getInstance();
            d1 = dateFormat.parse(startTime);
            d2 = dateFormat.parse(endTime);
            // 结束时间加一天
            cal.setTime(d2);
            cal.add(Calendar.DATE, +1);
            d2 = cal.getTime();
            // 如果开始时间小于结束时间则执行
            while (d1.before(d2)) {
                // 循环讲数据插入到list
                dates.add(dateFormat.format(d1));
                cal.setTime(d1);
                cal.add(Calendar.DAY_OF_MONTH, 1);
                d1 = cal.getTime();
            }
        } catch (ParseException e) {
            logger.info("时间格式有误");
            return null;
        }

        return dates;
    }

    /**
     * 获取今日小时数（包括当前小时）
     *
     * @return 返回int类型数值
     */
    public static List<Integer> hours() {
        Calendar cal = Calendar.getInstance();
        Integer curHour = cal.get(Calendar.HOUR_OF_DAY);
        List<Integer> hours = new ArrayList<Integer>();
        for (int i = 0; i <= curHour; i++) {
            hours.add(i);
        }
        return hours;
    }

    /**
     * 判断传入的是不是当前日期
     *
     * @param date 查询时间
     * @return 是返回true，不是返回false
     */
    public static boolean currentDate(String date) {
        // 当前时间
        Date now = new Date();
        // 获取今天的日期
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        String nowDay = dateFormat.format(now);

        return date.equals(nowDay);
    }

    /**
     * 获取当前日期30天前的日期
     *
     * @return 返回一个格式化后的时间
     */
    @SuppressWarnings("static-access")
    public static String currentDateTo30() {
        Calendar theCa = Calendar.getInstance();
        // 获取当前日期
        theCa.setTime(new Date());
        // 获取30天前的日期
        theCa.add(Calendar.DATE, -30);
        Date date = theCa.getTime();
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        String nowDay = dateFormat.format(date);
        return nowDay;
    }

    /**
     * 获取指定日期30天前的日期
     *
     * @return 返回一个格式化后的时间
     */
    public static String dateTo30(String selectDate) {
        Date date = new Date();

        String nowDay = "";

        try {
            Calendar cal = Calendar.getInstance();
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
            // 字符串转换为日期格式
            date = dateFormat.parse(selectDate);
            cal.setTime(date);
            // 减去30天
            cal.add(Calendar.DATE, -30);
            date = cal.getTime();
            nowDay = dateFormat.format(date);
        } catch (ParseException e) {
            logger.info("时间格式有误");
            return null;
        }
        return nowDay;
    }

    /**
     * 判断传入日期是否在当前时间到30天前的日期段内
     *
     * @param selectDate 查询时间
     * @return 是返回true，不是返回false
     */
    public static boolean atDateInterval(String selectDate) {
        boolean result = false;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            Date date = new Date();
            Date dateTo30 = dateFormat.parse(currentDateTo30());
            Date sdate = dateFormat.parse(selectDate);
            if (sdate.after(dateTo30) && sdate.before(date)) {
                result = true;
            }
        } catch (ParseException e1) {
            logger.info("时间格式有误");
            return false;
        }
        return result;
    }

    /**
     * 判断传入的日期是不是大于当前日期
     *
     * @param selectDate 查询时间
     * @return 是返回true，不是返回false
     */
    public static boolean afterToday(String selectDate) {
        boolean result = false;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            Date date = new Date();
            Date sdate = dateFormat.parse(selectDate);
            if (sdate.after(date)) {
                result = true;
            }
        } catch (ParseException e1) {
            logger.info("时间格式有误");
            return false;
        }
        return result;
    }

    /**
     * 判断传入的日期是不是大于当前日期
     *
     * @param selectDate 查询时间
     * @return 是返回true，不是返回false
     */
    public static boolean afterTodayY_M_D_H_S(Date selectDate) {
        boolean result = false;
        SimpleDateFormat dateFormat = new SimpleDateFormat(DateUtils.Y_M_D_H_M_S);
        try {
            Date date = new Date();
            Date sdate = dateFormat.parse(dateFormat.format(selectDate));
            if (sdate.after(date)) {
                result = true;
            }
        } catch (ParseException e1) {
            logger.info("时间格式有误");
            return false;
        }
        return result;
    }

    /**
     * 判断传入的两个日期的大小(仅限不相等的两个日期比较)
     *
     * @return 是返回true，不是返回false
     */
    public static boolean dateCompare(String startDate, String endDate) {
        boolean result = false;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            Date date = dateFormat.parse(endDate);
            Date sdate = dateFormat.parse(startDate);
            if (date.after(sdate)) {
                result = true;
            }
        } catch (ParseException e1) {
            logger.info("时间格式有误");
            return false;
        }
        return result;
    }

    /**
     * 判断传入的两个日期是否相等
     *
     * @return 是返回true，不是返回false
     */
    public static boolean dateEquals(String startDate, String endDate) {
        boolean result = false;
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        try {
            Date date = dateFormat.parse(endDate);
            Date sdate = dateFormat.parse(startDate);
            if (date.equals(sdate)) {
                result = true;
            }
        } catch (ParseException e1) {
            logger.info("时间格式有误");
            return false;
        }
        return result;
    }

    /**
     * 时间检查(是否符合格式--开始时间是否小于结束时间--结束时间是否小于当前时间--开始时间是否大于系统上线时间【系统上线时间在配置文件修改】)
     *
     * @param startTime 开始时间
     * @param endTime   结束时间
     * @return 正确返回true，错误返回false
     */
    public static boolean isRight(String startTime, String endTime, String systemDate) {
        boolean result = false;
        int days = -1;
        result = isValidDate(startTime);
        if (result) {
            result = isValidDate(endTime);
            if (result) {
                days = daysBetween(startTime, endTime);
                if (days > 0) {
                    result = afterToday(endTime);
                    if (!result) {
                        days = daysBetween(systemDate, startTime);
                        if (days > 0) {
                            result = true;
                        } else {
                            result = false;
                        }
                    } else {
                        result = false;
                    }
                } else {
                    result = false;
                }
            }
        }

        return result;
    }

    /**
     * 将长时间格式字符串转换为时间 yyyy-MM-dd HH:mm:ss
     *
     * @param strDate
     * @return
     */
    public static Date strToDateLong(String strDate) {
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        ParsePosition pos = new ParsePosition(0);
        Date strtodate = formatter.parse(strDate, pos);
        return strtodate;
    }

    /**
     * 返回时间百分比
     *
     * @param dateStr1
     * @param dateStr2
     * @return
     */
    public static String returnTimePercentage(String dateStr1, String dateStr2) {
        Date currentDate = new Date();
        Date date1 = null;
        Date date2 = null;
        date1 = DateUtils.strToDateLong(dateStr1);
        date2 = DateUtils.strToDateLong(dateStr2);

        // 使用大数据格式处理日期格式
        BigDecimal b1 = new BigDecimal(date1.getTime());
        BigDecimal b2 = new BigDecimal(date2.getTime());
        BigDecimal bc = new BigDecimal(currentDate.getTime());
        DecimalFormat df = new DecimalFormat();

        // 减法
        BigDecimal bMiuns = bc.subtract(b1);
        BigDecimal bMiunsAll = b2.subtract(b1);
        // 除法
        BigDecimal bOk = bMiuns.divide(bMiunsAll, 2, RoundingMode.HALF_UP);
        String bOk1 = df.format(bOk);
        return bOk1;
    }

    /**
     * 判断传入的日期是不是大于当前日期
     *
     * @param selectDate 查询时间
     * @return 是返回true，不是返回false
     */
    public static boolean beforeDate(String selectDate) {
        boolean result = false;
        SimpleDateFormat dateFormat2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        try {
            Date date = new Date();
            Date sdate = dateFormat2.parse(selectDate);
            if (sdate.before(date)) {
                result = true;
            }
        } catch (ParseException e1) {
            logger.info("时间格式有误");
            return false;
        }
        return result;
    }

    /**
     * date2比date1多的天数
     *
     * @return
     */
    public static int differentDays(String selectDate1, String selectDate2) {
        try {
            SimpleDateFormat dateFormat2 = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            Date date1 = dateFormat2.parse(selectDate1);
            Date date2 = dateFormat2.parse(selectDate2);
            Calendar cal1 = Calendar.getInstance();
            cal1.setTime(date1);
            Calendar cal2 = Calendar.getInstance();
            cal2.setTime(date2);
            int day1 = cal1.get(Calendar.DAY_OF_YEAR);
            int day2 = cal2.get(Calendar.DAY_OF_YEAR);
            int year1 = cal1.get(Calendar.YEAR);
            int year2 = cal2.get(Calendar.YEAR);
            if (year1 != year2) // 同一年
            {
                int timeDistance = 0;
                for (int i = year1; i < year2; i++) {
                    if (i % 4 == 0 && i % 100 != 0 || i % 400 == 0) // 闰年
                    {
                        timeDistance += 366;
                    } else // 不是闰年
                    {
                        timeDistance += 365;
                    }
                }
                return timeDistance + (day2 - day1);
            } else // 不同年
            {
                return day2 - day1;
            }
        } catch (ParseException e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static Date parse(String dateStr, String pattern) {
        return parse(dateStr, null, pattern);
    }

    public static Date parse(String dateStr, TimeZone timeZone, String pattern) {
        if (StringUtils.isBlank(dateStr)) {
            return null;
        }

        String[] parsePatterns = (String[]) null;
        if (StringUtils.isNotBlank(pattern)) {
            parsePatterns = new String[]{pattern, "yyyy-MM-dd",
                    "yyyy-MM-dd HH:mm", "yyyy-MM-dd HH:mm:ss"};
        } else {
            parsePatterns = new String[]{"yyyy-MM-dd", "yyyy-MM-dd HH:mm",
                    "yyyy-MM-dd HH:mm:ss","yyyy/MM/dd HH:mm:ss"};
        }
        try {
            SimpleDateFormat parser = null;
            ParsePosition pos = new ParsePosition(0);

            for (int i = 0; i < parsePatterns.length; ++i) {
                parser = new SimpleDateFormat(parsePatterns[i]);
                parser.applyPattern(parsePatterns[i]);

                if (timeZone != null) {
                    parser.setTimeZone(timeZone);
                }

                pos.setIndex(0);
                Date date = parser.parse(dateStr, pos);
                if ((date != null) && (pos.getIndex() == dateStr.length())) {
                    return date;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return null;
    }

    public static String format(Date date, String pattern) {
        if (date == null) {
            return null;
        }
        TimeZone timeZone = TimeZone.getDefault();
        return DateFormatUtils.format(date, pattern, timeZone);
    }

    //获取本周的开始时间
    @SuppressWarnings("unused")
    public static Date getBeginDayOfWeek() {
        Date date = new Date();
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        int dayofweek = cal.get(Calendar.DAY_OF_WEEK);
        if (dayofweek == 1) {
            dayofweek += 7;
        }
        cal.add(Calendar.DATE, 2 - dayofweek);
        return cal.getTime();
    }

    //获取本周的结束时间
    public static String getEndDayOfWeek() {
        Calendar cal = Calendar.getInstance();
        cal.setTime(getBeginDayOfWeek());
        cal.add(Calendar.DAY_OF_WEEK, 7);
        Date weekEndSta = cal.getTime();
        return DateUtils.format(weekEndSta, DateUtils.Y_M_D);
    }

    /**
     * 获取精确到秒的时间戳
     *
     * @return
     */
    public static String getSecondTimestamp(Date date) {
        if (null == date) {
            return "";
        }
        String timestamp = String.valueOf(date.getTime());
        int length = timestamp.length();
        if (length > 3) {
            return String.valueOf(Integer.valueOf(timestamp.substring(0, length - 3)));
        } else {
            return "";
        }
    }


    /**
     * * 将短时间格式字符串转换为时间 yyyy-MM-dd
     * *
     * * @param strDate
     * * @return
     */
    public static Date strToDate(String strDate) {
        return strToDate(strDate,Y_M_D);
    }

    /**
     * * 将短时间格式字符串转换为时间 yyyy-MM-dd
     * *
     * * @param strDate
     * * @return
     */
    public static Date strToDate(String strDate,String pattern) {
        SimpleDateFormat formatter = new SimpleDateFormat(pattern);
        ParsePosition pos = new ParsePosition(0);
        Date strToDate = formatter.parse(strDate, pos);
        return strToDate;
    }
    /**
     * * 将短时间格式字符串转换为时间 yyyy-MM-dd
     * *
     * * @param strDate
     * * @return
     */
    public static Date stringToDate(String strDate) {
        if(!StringUtils.isEmpty(strDate)){
            try {
                return strToDate(strDate,Y_M_D_H_M_S);
            }catch (Exception e){
                logger.error("字符串转Date出错，待转换字符串是:{},异常信息是:{}",strDate,e);
            }
        }
        return null;
    }

    /**
     * date to localDate
     *
     * @param date
     * @return
     */
    public static LocalDate dateToLocalDate(Date date) {
        Instant instant = date.toInstant();
        ZoneId zoneId = ZoneId.systemDefault();
        LocalDate localDate = instant.atZone(zoneId).toLocalDate();
        return localDate;
    }

    /**
     * localDate to date
     *
     * @param
     * @return
     */
    public static Date localDateToDate(LocalDate localDate) {
        ZoneId zoneId = ZoneId.systemDefault();
        ZonedDateTime zdt = localDate.atStartOfDay(zoneId);
        Date date = Date.from(zdt.toInstant());
        return date;
    }

    /**
     * * 将传入的时间增加number天
     * *
     * * @return
     */
    @SuppressWarnings("static-access")
    public static Date addDate(Date date, int number) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date); //需要将date数据转移到Calender对象中操作
        calendar.add(Calendar.DATE, number);//把日期往后增加n天.正数往后推,负数往前移动
        date = calendar.getTime();   //这个时间就是日期往后推一天的结果
        return date;
    }

    /**
     * 将yyyy-MM-dd格式转化成yyyy-MM-dd hh:mm:ss
     *
     * @param date
     * @return
     */
    public static Date addDateSeconds(Date date) {
        String s = new SimpleDateFormat("yyyy-MM-dd").format(date);
        Date d = null;
        try {
            DateFormat df = new SimpleDateFormat("hh:mm:ss");
            s = s + " " + df.format(new Date());
            DateFormat fullDf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
            d = fullDf.parse(s);
        } catch (ParseException e) {
            e.printStackTrace();
        }
        return d;
    }

    /**
     * 获取当前日期-没有分隔符
     *
     * @return
     */
    public static String getCurrentDate() {
        return LocalDate.now().toString().replace("-", "");
    }

}
