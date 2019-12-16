package com.bee.platform.common.utils;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.config.AuthRestTemplateConfig;
import com.bee.platform.common.config.property.AuthConfigProperties;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author liang.li
 * @ClassName UserInfoUtils
 * @Description 获取当前登录用户信息
 * @Date 2018-12-27 14:29
 */
@Slf4j
@Component("authUserInfoUtils")
public class UserInfoUtils {

    @Autowired
    private AuthRestTemplateConfig restTemplate;

    @Autowired
    private AuthConfigProperties props;

    /**
     * contextPath
     */
    private static final String CONTEXT_PATH="/platform-cloudmanufactureuser";
    /**
     * 获取用户信息接口
     */
    private static final String USER_INFO_PATH = "/platform/auth/getUserInfo";
    /**
     * 通过用户id查询用户信息
     */
    private static final String GET_USER_BY_ID = "/platform/auth/user/";
    /**
     * 通过用户id批量查询用户信息
     */
    private static final String GET_BY__USERIDS= "/platform/auth/simple/users";

    /**
     * @notes: 从用户服务获取用户信息
     * @Author: junyang.li
     * @Date: 16:05 2019/9/19
     * @param sysToken : 用户登录凭证
     * @return: com.bee.platform.common.entity.AuthPlatformUserInfo
     */
    public AuthPlatformUserInfo getUserInfo(String sysToken){
        //判空
        if(StringUtils.isEmpty(sysToken)){
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,
                    ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        String url=props.getAddress()+CONTEXT_PATH+USER_INFO_PATH+"?sysToken="+sysToken;
        HttpHeaders headers=new HttpHeaders();
        headers.set("sysToken",sysToken);
        JSONObject object=restTemplate.sendRestGet(url,headers);
        //判空
        if(object==null){
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,
                    ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        //获得用户信息
        JSONObject userObject=object.getJSONObject("object");
        //判空
        if(userObject!=null){
            try {
                return userObject.toJavaObject(AuthPlatformUserInfo.class);
            }catch (JSONException e){
                log.error("获取用户信息异常，数据返回信息是:{},异常信息是:{}",userObject,e);
            }
        }
        throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,
                ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
    }
    /**
     * @notes: 根据用户id查询用户详细
     * @Author: junyang.li
     * @Date: 13:45 2019/10/31
     * @param sysToken : 当前操作人凭证
     * @param userId : 待查询用户id
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.cloud.user.entity.AuthPlatformUser>
     */
    public AuthPlatformUserInfo getUserById(String sysToken, Integer userId){
        if(StringUtils.isEmpty(sysToken)|| userId==null){
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,
                    ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        String url=props.getAddress()+CONTEXT_PATH+GET_USER_BY_ID+userId;
        HttpHeaders headers=new HttpHeaders();
        headers.set("sysToken",sysToken);
        JSONObject object=restTemplate.sendRestGet(url,headers);
        this.checkResult(object);
        return object.getJSONObject("object").toJavaObject(AuthPlatformUserInfo.class);
    }
    /**
     * @notes: 通过用户id查询用户详细
     * @Author: junyang.li
     * @Date: 14:46 2019/11/6
     * @param sysToken : 当前操作人登录凭证
     * @param userIds : 待查询用户id
     * @return: java.util.Map<java.lang.Integer,com.bee.platform.common.entity.AuthPlatformUserInfo>
     */
    public Map<Integer,AuthPlatformUserInfo> getUserById(String sysToken, List<Integer> userIds){
        //判空
        if(StringUtils.isEmpty(sysToken)|| CollectionUtils.isEmpty(userIds)){
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,
                    ExceptionMessageEnum.FAILED_TO_GET_USER_INFO);
        }
        String url=props.getAddress()+CONTEXT_PATH+GET_BY__USERIDS;
        HttpHeaders headers=new HttpHeaders();
        headers.set("sysToken",sysToken);
        Map<String, Object> map=new HashMap<>(2);
        map.put("userIds",userIds);
        JSONObject object=restTemplate.jsonPost(url,map,headers);
        this.checkResult(object);
        Map<String,Object> json=object.getJSONObject("object").getInnerMap();
        Map<Integer,AuthPlatformUserInfo> result=new HashMap<>(16);
        //遍历
        for (Map.Entry<String,Object> item:json.entrySet()) {
            Integer userId=Integer.valueOf(item.getKey());
            JSONObject obj=(JSONObject)item.getValue();
            AuthPlatformUserInfo userInfo=obj.toJavaObject(AuthPlatformUserInfo.class);
            result.put(userId,userInfo);
        }
        //获得返回结果
        return result;
    }
    /**
     * @notes: 校验返回的json结果
     * @Author: junyang.li
     * @Date: 14:21 2019/11/6
     * @param object : json对象
     * @return: void
     */
    private void checkResult(JSONObject object){
        //判空
        if(object==null){
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_USER_INFO,
                    ExceptionMessageEnum.ERROR_SYSTEM);
        }
        Integer code=object.getIntValue("code");
        if(!ResCodeEnum.SUCCESS.getCode().equals(code)){
            throw new BusinessException(ResCodeEnum.SUCCESS,
                    object.getString("message"));
        }
    }

}
