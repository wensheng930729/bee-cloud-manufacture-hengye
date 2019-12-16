package com.bee.platform.common.service;

/**
 * @author liang.li
 * @ClassName RedisSequenceService
 * @Description 从redis中获取自增序列
 * @Date 2019/8/6 15:34
 */
public interface RedisSequenceService {

    /**
     * @param prefix   编码前缀
     * @param redisKey redis的键
     * @param length   自增序列的长度
     * @param expire   是否过期
     * @return 组装后的编码
     */
    //public String getCode(String prefix, String redisKey, Integer length, Boolean expire);

    /**
     * 根据个数在前面用0补齐
     *
     * @param code
     * @param length 总长度
     * @return
     */
    public String getAddCode(String code, Integer length);

    /**
     * 通过redisKey从redis获取value
     *
     * @param redisKey
     * @param expire   是否过期
     * @return
     */
    //public Integer getRedisValue(String redisKey, Boolean expire);

}
