package com.bee.platform.common.utils;

import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.text.DecimalFormat;

/**
 * @description:  BigDecimal工具类
 * @author: junyang.li
 * @create: 2019-02-01 15:28
 **/
public class BigDecimalUtils {

    public static final String TWO_DECIMAL_PLACES="##0.00";

    /**
     *  one thousand
     */
    public static final BigDecimal THOUSAND=new BigDecimal(1000);
    /**
     * 保留三位小数
     */
    public static final int THREE_SCALE=3;
    /**
     * @notes 格式化decimal数据
     * @Author junyang.li
     * @Date 15:33 2019/2/1
     **/
    public static String format(String pattern, BigDecimal bigDecimal){
        if(StringUtils.isEmpty(pattern)||bigDecimal==null){
            return null;
        }
        return new DecimalFormat(pattern).format(bigDecimal);
    }


    /**
     * @notes: 多参数加法
     * @Author: junyang.li
     * @Date: 15:54 2019/7/24
     * @param args :
     * @return: java.math.BigDecimal
     */
    public static BigDecimal add(BigDecimal total,BigDecimal... args){
        if(total==null){
            total=BigDecimal.ZERO;
        }
        //判空
        if(args!=null) {
            //遍历
            for (BigDecimal item:args) {
                total=total.add(isNull(item));
            }
        }
        return total;
    }

    /**
     * @notes: BigDecimal 类型判空，如果为空，返回0
     * @Author: junyang.li
     * @Date: 15:50 2019/7/24
     * @return: java.math.BigDecimal
     */
    public static BigDecimal isNull(BigDecimal bigDecimal){
        return bigDecimal==null?BigDecimal.ZERO:bigDecimal;
    }

    /**
     * @notes: String 类型转换为BigDecimal
     * @Author: junyang.li
     * @Date: 10:04 2019/7/26
     * @param str :
     * @return: java.math.BigDecimal
     */
    public static BigDecimal strToDecimal(String str){
        if(!StringUtils.isEmpty(str)){
            return new BigDecimal(str);
        }
        return BigDecimal.ZERO;
    }

    /**
     * @notes: 除法
     * @Author: junyang.li
     * @Date: 10:29 2019/7/31
     * @param dividend : 被除数
     * @param divisor : 除数
     * @param divisor : 保留小数位
     * @return: java.math.BigDecimal
     */
    public static BigDecimal divide(BigDecimal dividend,BigDecimal divisor,int scale){
        if(BigDecimal.ZERO.compareTo(divisor)==0){
            throw new RuntimeException("除数不能为0");
        }
        return dividend.divide(divisor,scale,BigDecimal.ROUND_HALF_UP);
    }

    /**
     * @notes:  减法
     * @Author: junyang.li
     * @Date: 13:20 2019/7/25
     * @param decimal : 减数
     * @param subtrahend : 被减数
     * @return: java.math.BigDecimal
     */
    public static BigDecimal subtract(BigDecimal decimal,BigDecimal subtrahend){
        return isNull(decimal).subtract(isNull(subtrahend));
    }
}
