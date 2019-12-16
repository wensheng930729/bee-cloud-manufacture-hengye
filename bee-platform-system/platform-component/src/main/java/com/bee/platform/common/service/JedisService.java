package com.bee.platform.common.service;

import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import com.fasterxml.jackson.core.type.TypeReference;

/**
 * @ClassName ComponentJedisService
 * @Description 操作RedisTemplate的接口
 * @author zhigang.zhou
 * @Date 2018年11月30日 下午1:16:35
 * @version 1.0.0
 */
public interface JedisService {

    boolean exists(String key);

    String set(String key, String value, int seconds);

    String getSet(String key, String value, int seconds);

    String get(String key);

    Long geoadd(String key, double longitude, double latitude, byte[] obj);

    void delKey(String key);

    void delNativeKey(String key);

    Map<String ,Object> getMapData(String key);

    boolean lock(String key, int seconds);

    void unlock(String key);

    String getLocakValue(String key);

    <T> T getObject(String key, Class<T> clz);
    
    <T> T getObject(String str, TypeReference<T> jsonTypeReference);
    
    <T> T getJsonObject(String key, Class<T> clazz);
    
    <T> List<T> getJsonArrayObject(String key, Class<T> clazz);
    
    <T> T setJsonObject(String key, T t, int seconds);

    void setObject(String key, Object obj, int time);

    Integer incr(String key,int time);

    void setIncr(String key, Integer value);

    <HK, HV>   void setHash(Object key, Map<? extends HK, ? extends HV> map);

    <HK, HV>   void setHash(Object key, Map<? extends HK, ? extends HV> map,int seconds);

    <T> T getHash(Object redisKey, Object objectKey, Class<T> tClass);

    String getHashToJsonStr(Object redisKey, Object objectKey);
    
    Long ttl(String key, TimeUnit unit);
}
