package com.bee.platform.common.utils;

import com.alibaba.fastjson.JSONException;
import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.config.AuthRestTemplateConfig;
import com.bee.platform.common.config.property.AuthConfigProperties;
import com.bee.platform.common.entity.AuthCustomerNameDTO;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

/**
 * @author liang.li
 * @ClassName UserInfoUtils
 * @Description 获取当前登录用户信息
 * @Date 2018-12-27 14:29
 */
@Slf4j
@Component("customerInfoUtils")
public class CustomerInfoUtils {

    @Autowired
    private AuthRestTemplateConfig restTemplate;

    @Autowired
    private AuthConfigProperties props;

    /**
     * contextPath
     */
    private static final String CONTEXT_PATH="/platform-cloudmanufactureuser";
    /**
     * 获取客户信息接口
     */
    private static final String CUSTOMER_INFO_PATH = "/customer/getCustomerNameById";

    /**
     * @notes: 根据id查询客户名称
     * @Author: junyang.li
     * @Date: 16:05 2019/9/19
     * @param id : id
     * @return: AuthCustomerNameDTO
     */
    public AuthCustomerNameDTO getCustomerInfo(Integer id){
        //判空
        if(ObjectUtils.isEmpty(id)){
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER,
                    ExceptionMessageEnum.SYSTEM_INVALID_PARAMS);
        }
        String url=props.getAddress()+CONTEXT_PATH+CUSTOMER_INFO_PATH+"?id="+id;
        HttpHeaders headers=new HttpHeaders();
        headers.set("id",id.toString());
        JSONObject object=restTemplate.sendRestGet(url,headers);
        //判空
        if(object==null){
            throw new BusinessException(ResCodeEnum.FAILED_TO_GET_CUSTOMER,
                    ExceptionMessageEnum.FAILED_TO_GET_CUSTOMER);
        }
        //获得用户信息
        JSONObject customerObject=object.getJSONObject("object");
        //判空
        if(customerObject!=null){
            try {
                return customerObject.toJavaObject(AuthCustomerNameDTO.class);
            }catch (JSONException e){
                log.error("获取客户信息异常，数据返回信息是:{},异常信息是:{}",customerObject,e);
            }
        }
        throw new BusinessException(ResCodeEnum.FAILED_TO_GET_CUSTOMER,
                ExceptionMessageEnum.FAILED_TO_GET_CUSTOMER);
    }
}
