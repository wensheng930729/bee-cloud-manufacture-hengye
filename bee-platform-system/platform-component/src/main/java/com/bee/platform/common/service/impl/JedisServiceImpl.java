package com.bee.platform.common.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.support.atomic.RedisAtomicLong;
import org.springframework.stereotype.Service;
import redis.clients.jedis.exceptions.JedisException;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * @ClassName ComponentJedisService
 * @Description 操作RedisTemplate的接口
 * @author zhigang.zhou
 * @Date 2018年11月30日 下午1:16:35
 * @version 1.0.0
 */
@Slf4j
@Service
public class JedisServiceImpl implements JedisService {

	@Autowired
    private RedisTemplate<Object, Object> redisTemplate;

	@Autowired
	private ConfigService configService;

    @Override
    public boolean exists(String key) {
        Boolean result = redisTemplate.hasKey(key);
        return null == result ? false : result;
    }

    @Override
    public String set(String key, String value, int seconds) {
        redisTemplate.opsForValue().set(key, value);
        if (seconds != 0){
            redisTemplate.expire(key, seconds, TimeUnit.SECONDS);
        }
        return key;
    }

    @Override
    public String getSet(String key, String value, int seconds) {
        redisTemplate.opsForValue().set(key, value);
        redisTemplate.expire(key, seconds, TimeUnit.SECONDS);
        return key;
    }

    @Override
    public String get(String key) {
        Object val = redisTemplate.opsForValue().get(key);
        if(val != null && val instanceof String) {
            return (String) val;
        }
        if(val != null && val instanceof Integer) {
            return String.valueOf(val);
        }
        if(val != null && val instanceof Long) {
            return String.valueOf(val);
        }
        return  (String) val;
    }

    @Override
    public Long geoadd(String key, double longitude, double latitude, byte[] obj) {
        return null;
    }

    @Override
    public void delKey(String key) {
        redisTemplate.delete(key);
    }

    @Override
    public void delNativeKey(String key) {
    	redisTemplate.delete(key);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> getMapData(String key) {
        String str = (String) redisTemplate.opsForValue().get(key);
        Map<String, Object> map = JSON.parseObject(str, Map.class);
        return map;
    }

    /**
     * @Description: 如为第一次，则加上锁，每次调用值会自动加1
     */
    @Override
    public boolean lock(String key, int seconds) {
    	Long increment = redisTemplate.opsForValue().increment(key, 1);
        if (increment != null && increment == 1) {
        	redisTemplate.expire(key, seconds, TimeUnit.SECONDS);
            return false;
        }
        return true;
    }

    @Override
    public void unlock(String key) {
        log.debug("key=" + key);
     	redisTemplate.delete(key);
    }

    @Override
    public String getLocakValue(String key) {
        return (String) redisTemplate.opsForValue().get(key);
    }

    @Override
    public  <T> T getObject(String key,Class<T> clz) {
        String str = get(key);
        if(StringUtils.isBlank(str)){
            return null;
        }
        ObjectMapper mapper = new ObjectMapper();
        mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
        T t = null;
        try {
            t = mapper.readValue(str, clz);
        } catch (IOException e) {
            log.error("jackson解析json出错",e);
        }
        return t;
    }

    @Override
    public void setObject(String key,Object obj,int time){
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
        String json = null;
        try {
            json = objectMapper.writeValueAsString(obj);
        } catch (JsonProcessingException e) {
            log.error("jackson转换对象为json出错",e);
        }
        if(StringUtils.isNotBlank(json)) {
            set(key,json,time);
        }
    }

    @Override
    public Integer incr(String key,int time){
        log.info("incr add key=" + key);
        RedisConnectionFactory factory=redisTemplate.getConnectionFactory();
        if(factory==null){
            throw new RuntimeException("redis 连接异常,无法获得RedisConnectionFactory对象。");
        }
        RedisAtomicLong entityIdCounter = new RedisAtomicLong(key, factory);
        Long increment = entityIdCounter.incrementAndGet();
        //初始设置过期时间
        entityIdCounter.expire(time, TimeUnit.SECONDS);
        return increment.intValue();
    }


    @Override
    public void setIncr(String key, Integer value) {
       try {
           RedisConnectionFactory factory=redisTemplate.getConnectionFactory();
           if(factory!=null && value!=null){
               RedisAtomicLong entityIdCounter = new RedisAtomicLong(key, factory);
               entityIdCounter.set(value.longValue());
           }
       }catch (Exception e){
           log.info("redis 插入键:{}，和值:{}失败，异常信息是:{}",key,value,e);
       }
    }

    @Override
    public<HK, HV>   void setHash(Object key, Map<? extends HK, ? extends HV> map){
        HashOperations<Object, HK, HV> hashOperations=redisTemplate.opsForHash();
        hashOperations.putAll(key,map);
    }

    @Override
    public <HK, HV> void setHash(Object key, Map<? extends HK, ? extends HV> map, int seconds) {
        HashOperations<Object, HK, HV> hashOperations=redisTemplate.opsForHash();
        hashOperations.putAll(key,map);
        if (seconds != 0){
            redisTemplate.expire(key, seconds, TimeUnit.SECONDS);
        }
    }

    @Override
    public<T> T getHash(Object redisKey,Object objectKey,Class<T> tClass){
        String value=this.getHashToJsonStr(redisKey,objectKey);
        if(!StringUtils.isEmpty(value)){
            return JSONObject.parseObject(value,tClass);
        }
        return null;
    }


    @Override
    public String getHashToJsonStr(Object redisKey, Object objectKey) {
        Object object=redisTemplate.opsForHash().get(redisKey,objectKey);
        if(object!=null){
            return String.valueOf(object);
        }
        return null;
    }

    @Override
    public <T> T getObject(String str, TypeReference<T> jsonTypeReference) {
        ObjectMapper mapper = new ObjectMapper();
        mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
        T t = null;
        try {
            t = mapper.readValue(str, jsonTypeReference);
        } catch (IOException e) {
            log.error("获取数据发生异常,e={}", e);
        }
        return t;
    }

	@Override
	public <T> T getJsonObject(String key, Class<T> clazz) {
        String resString = get(key);
        if(StringUtils.isNotBlank(resString)) {
            return JSON.parseObject(resString, clazz);
        }
		return null;
	}

	@Override
	public <T> T setJsonObject(String key, T t, int seconds) {
        String jsonString = JSON.toJSONString(t);
        set(key, jsonString, seconds);
        return t;
	}

	@Override
	public <T> List<T> getJsonArrayObject(String key, Class<T> clazz) {
        String resString = get(key);
        if(StringUtils.isNotBlank(resString)) {
            return JSON.parseArray(resString, clazz);
        }
		return null;
	}

	@Override
	public Long ttl(String key, TimeUnit unit) {
	    return redisTemplate.getExpire(key, TimeUnit.SECONDS);
	}

	
}
