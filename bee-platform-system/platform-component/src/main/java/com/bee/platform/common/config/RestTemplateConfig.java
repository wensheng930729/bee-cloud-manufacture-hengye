package com.bee.platform.common.config;

import java.util.Map;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.client.ClientHttpRequestFactory;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import com.alibaba.fastjson.JSONObject;

import lombok.extern.slf4j.Slf4j;

/**
 * @ClassName RestTemplateConfig
 * @Description 发请求的组件
 * @author zhigang.zhou
 * @Date 2018年11月27日 下午1:16:35
 * @version 1.0.0
 */
@Slf4j
@Configuration
public class RestTemplateConfig {

    @Bean
    public RestTemplate restTemplate(ClientHttpRequestFactory factory) {
        return new RestTemplate(factory);
    }

    @Bean
    public ClientHttpRequestFactory simpleClientHttpRequestFactory() {
        SimpleClientHttpRequestFactory factory = new SkipSslVerificationHttpRequestFactory();
        // 单位为ms
        factory.setReadTimeout(5000);
        // 单位为ms
        factory.setConnectTimeout(5000);
        return factory;
    }

    public JSONObject sendRestGet(String url, HttpHeaders headers){
        if(headers==null){
            headers=new HttpHeaders();
        }
        HttpEntity<String> httpEntity = new HttpEntity<>(null,headers);
        ResponseEntity<JSONObject> object;
        try {
            object = restTemplate(simpleClientHttpRequestFactory()).exchange(url, HttpMethod.GET, httpEntity, JSONObject.class);
        }catch (Exception e){
            log.error("连接异常。连接url是:{}  异常信息是:",url,e);
            return null;
        }
        return object.getBody();
    }
    /**
     * @notes json串提交
     * @Author junyang.li
     * @Date 19:14 2019/1/18
     **/
    public JSONObject sendJsonPost(String url, String value, HttpHeaders headers){
        if(headers==null){
            headers=new HttpHeaders();
        }
        headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
        HttpEntity<String> requestEntity = new HttpEntity<>(value,headers);
        try {
            return restTemplate(simpleClientHttpRequestFactory()).postForObject(url,requestEntity,JSONObject.class);
        }catch (Exception e){
            log.error("连接异常。连接url是:{} ,请求参数是:{} , 异常信息是:",url,value,e);
        }
        return null;
    }
    /**
     * @notes Payload提交
     * @Author junyang.li
     * @Date 17:09 2019/2/28
     **/
    public JSONObject jsonPost(String url, Map<String, Object> map, HttpHeaders headers){
        if(headers==null){
            headers=new HttpHeaders();
        }
        headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
        HttpEntity<Map<String, Object>> request = new HttpEntity<>(map, headers);
        try {
            return restTemplate(simpleClientHttpRequestFactory()).postForObject(url,request,JSONObject.class);
        }catch (Exception e){
            log.error("连接异常。连接url是:{} ,请求参数是:{} , 异常信息是:",url,JSONObject.toJSONString(map),e);
        }
        return null;
    }
    /**
     * @notes 表单提交
     * @Author junyang.li
     * @Date 19:13 2019/1/18
     **/
    public JSONObject sendPost(String url, HttpHeaders header, MultiValueMap<String, String> map){
        if(header==null){
            header=new HttpHeaders();
        }
        header.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
        HttpEntity<MultiValueMap<String, String>> requestEntity = new HttpEntity<>(map,header);
        try {
            return restTemplate(simpleClientHttpRequestFactory()).postForObject(url,requestEntity,JSONObject.class);
        }catch (Exception e){
            log.error("连接异常。连接url是:{}  异常信息是:",url,e);
            return null;
        }
    }
}
