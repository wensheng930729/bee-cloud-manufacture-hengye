package com.bee.platform.common.utils;

import lombok.extern.slf4j.Slf4j;
import org.joda.time.DateTime;
import org.joda.time.Duration;

import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;

/**
 * @description:
 * @author: junyang.li
 * @create: 2018-12-05 10:27
 **/
@Slf4j
public class DateUtil {
    private static final DateTimeFormatter df = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    /**
     * @notes 两个日期的时间差
     * @Author junyang.li
     * @Date 18:21 2018/12/3
     **/
    public static long dateAlmost(Date end) {
        if (end == null) {
            return ConstantsUtil.ONE_DAY;
        }
        /*Instant endInst = end.toInstant();  //结束时间
        Instant startInst = new Date().toInstant();     //当前时间*/
        Duration d = new Duration(new DateTime(new Date()), new DateTime(end));
        return d.getStandardSeconds();
    }

    /**
     * @notes 当前时间加上秒
     * @Author junyang.li
     * @Date 18:21 2018/12/3
     **/
    public static Date plusSeconds(int seconds) {
        return new DateTime(new Date()).plusSeconds(seconds).toDate();
    }

    /**
     * 日期类型转换成字符串
     *
     * @param dateTime
     * @return
     * @Author fei.sun
     */
    public static String convertDateToString(LocalDateTime dateTime) {
        if (dateTime == null) {
            return null;
        }
        return df.format(dateTime);
    }

    /**
     * @Description LocalDateTime转换为Date
     * @author chenxm66777123
     * @Date 2019/10/21 11:07
     * @version 1.0.0
     */
    public static Date localDateTime2Date(LocalDateTime localDateTime) {
        ZoneId zoneId = ZoneId.systemDefault();
        ZonedDateTime zdt = localDateTime.atZone(zoneId);
        Date date = Date.from(zdt.toInstant());
        return date;
    }

    public static String date2Str(Date date) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        String result = sdf.format(date);
        return result;
    }
}
