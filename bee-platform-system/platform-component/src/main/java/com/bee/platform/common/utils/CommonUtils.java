package com.bee.platform.common.utils;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.ResCodeEnum;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import lombok.extern.slf4j.Slf4j;
import org.joda.time.DateTime;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;
import java.lang.ref.SoftReference;
import java.util.*;

/**
 * notes 工具类
 * author junyang.li
 * create 2018/11/7 0007 9:32
 **/
@Slf4j
public class CommonUtils {

    /**
     * @notes 判断字符是否为纯数字
     * @author junyang.li
     * @date 2018/11/7 0007 9:32
     */
    public static boolean isNumeric(String str){
        if(StringUtils.isEmpty(str)){return false;}
        for (int i=str.length();--i>=0;) {
            int chr=str.charAt(i);
            if(chr<48 || chr>57){
                return false;
            }
        }
        return true;
    }

    /**
     * @notes 判断是否为手机号
     * @author junyang.li
     * @date 2018/11/7 0007 9:56
     */
    public static  boolean isTelPhoneNum(String str){
        if(StringUtils.isEmpty(str)){return false;}
        return str.matches("^1\\d{10}");
    }

    /**
     * @notes 验证邮箱
     * @Author junyang.li
     * @Date 17:01 2018/11/21
     **/
    public static boolean isEmail(String email){
        if(StringUtils.isEmpty(email)){
            return false;
        }
        return email.matches(ConstantsUtil.V_EMAIL);
    }
    /**
     * @notes String 转Long
     * @Author junyang.li
     * @Date 17:01 2018/11/21
     **/
    public static Long strToLong(String str){
        try {
            return StringUtils.isEmpty(str)?null:Long.parseLong(str);
        }catch (NumberFormatException e){
            log.error("String 转换为Long 异常。String={0}",str);
        }
        return null;
    }

    /**
     * @notes String Integer
     * @Author junyang.li
     * @Date 17:01 2018/11/21
     **/
    public static Integer strToInteger(String str){
        try {
            return StringUtils.isEmpty(str)?null:Integer.parseInt(str);
        }catch (NumberFormatException e){
            log.error("String 转换为Integer 异常。String={0}",str);
        }
        return null;
    }

    /**
     * @notes 当前时间加上秒
     * @Author junyang.li
     * @Date 18:21 2018/12/3
     **/
    public static Date plusSeconds(Integer seconds) {
        if(seconds==null){
            return new Date();
        }
        return new DateTime(new Date()).plusSeconds(seconds).toDate();
    }

    /**
     * @notes 字符串拼接
     * @Author junyang.li
     * @Date 15:06 2019/1/21
     **/
    public static String splicing(Object...args){
        StringBuilder builder=new StringBuilder();
        if(args.length==0){
            return builder.toString();
        }
        for (int i = 0; i < args.length; i++) {
            builder.append(args[i]);
        }
        return builder.toString();
    }

    /**
     * @notes 隐藏手机号中的部分数字
     * @Author junyang.li
     * @Date 15:50 2019/3/4
     **/
    public static String disposePhoneNum(String phoneNum){
        if(phoneNum==null){
            return null;
        }
        if(!Validator.isMobile(phoneNum)){
            return phoneNum;
        }
        String subStr=phoneNum.substring(3,7);
        return phoneNum.replace(subStr,"****");
    }
    /**
     * @notes: 判断用户账户名是否为空，为空则返回隐藏手机号
     * @Author: junyang.li
     * @Date: 9:34 2019/5/23
     * @param nickName : 账户名
     * @param phone : 手机号
     * @return: java.lang.String
     */
    public static String getNickName(String nickName,String phone){
        return StringUtils.isEmpty(nickName)?disposePhoneNum(phone):nickName;
    }

    /**
     * @notes 从reques获得参数
     * @Author junyang.li
     * @Date 15:29 2018/12/2
     **/
    public  static String getParam(HttpServletRequest request, String key){
        if(request==null|| StringUtils.isEmpty(key)){
            return null;
        }
        String param=request.getHeader(key);
        return StringUtils.isEmpty(param)?request.getHeader(key.toUpperCase()):param;
    }

    /**
     * @notes 将数据转换为平台老数据
     * @Author junyang.li
     * @Date 20:00 2019/3/22
     **/
    public  static  Map<String,Object>  convertToData(Object obj, Pagination pagination){
        Map<String,Object> map=new SoftReference<Map<String,Object>>(new HashMap<>(16)).get();
        if(null != map) {
        	 map.put("content",obj);
             map.put("totalPages",pagination.getPages());
             map.put("totalElements",pagination.getTotal());
             map.put("size",pagination.getSize());
             map.put("number",pagination.getCurrent());
        }
        return map;
    }
    /**
     * 求并集
     * @param list1
     * @param list2
     * @param <T>
     * @return
     */
    public static<T> List<T> getUnion(List<T> list1, List<T> list2) {
        List<T> result = Lists.newArrayList();
        Set<T> set = new HashSet();
        set.addAll(list1);
        set.addAll(list2);
        result.addAll(set);
        return result;
    }

    /**
     * 取交集
     * @param list1
     * @param list2
     * @return
     */
    public static<T> List<T> getIntersection(List<T> list1, List<T> list2) {
        List<T> result = Lists.newArrayList();
        Set<T> set = new HashSet();
        set = Sets.intersection(Sets.newHashSet(list1), Sets.newHashSet(list2)).copyInto(set);
        result.addAll(set);
        return result;
    }

}
