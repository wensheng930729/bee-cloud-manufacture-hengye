package com.bee.platform.common.service.impl;

import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.RedisSequenceService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author liang.li
 * @ClassName RedisSequenceServiceImpl
 * @Description 从redis中获取自增序列
 * @Date 2019/8/6
 */
@Service
public class RedisSequenceServiceImpl implements RedisSequenceService {

    @Autowired
    private JedisService jedisService;

    /**
     * @param prefix   编码前缀
     * @param redisKey redis的键
     * @param length   自增序列的长度
     * @param expire   是否过期
     * @return 组装后的编码
     */
//    @Override
//    public String getCode(String prefix, String redisKey, Integer length, Boolean expire) {
//        Integer redisValue = getRedisValue(redisKey, expire);
//        String sequence = getAddCode(redisValue.toString(), length);
//        return prefix + sequence;
//    }

    /**
     * 根据个数在code前面用0补齐
     */
    @Override
    public String getAddCode(String code, Integer length) {
        if (StringUtils.isBlank(code)) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        int codeLen = code.length();
        if (codeLen < length) {
            int i1 = length - codeLen;
            for (int i = 0; i < i1; i++) {
                sb.append(0);
            }
            sb.append(code);
            return sb.toString();
        } else {
            return code;
        }
    }

    /**
     * 通过redisKey从redis获取value
     *
     * @param redisKey redis键
     * @param expire   是否过期
     * @return
     */
//    @Override
//    public Integer getRedisValue(String redisKey, Boolean expire) {
//        if (expire) {
//            return jedisService.incr(redisKey);
//        }
//        return jedisService.incrOne(redisKey);
//    }


}
