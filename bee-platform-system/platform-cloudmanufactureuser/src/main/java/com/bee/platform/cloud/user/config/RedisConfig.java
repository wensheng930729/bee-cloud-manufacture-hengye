package com.bee.platform.cloud.user.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.Jackson2JsonRedisSerializer;

import java.net.UnknownHostException;

/**
 * @ClassName RedisConfig
 * @Description  覆盖默认的自动配置
 * @author zhigang.zhou
 * @Date 2019年09月16日 下午13:16:35
 * @version 1.0.0
 */
@Configuration
public class RedisConfig {
	/**
	 * 修改默认的序列化规则
	 */
	@Bean
	public RedisTemplate<Object, Object> redisTemplate(RedisConnectionFactory redisConnectionFactory)
			throws UnknownHostException {
		RedisTemplate<Object, Object> template = new RedisTemplate<>();
		template.setConnectionFactory(redisConnectionFactory);
		// 1.创建序列化规则对象
		Jackson2JsonRedisSerializer<Object> jackson2JsonRedisSerializer = new Jackson2JsonRedisSerializer<>(
				Object.class);
		// 2.更改默认的序列化规则
		template.setDefaultSerializer(jackson2JsonRedisSerializer);
		return template;
	}
}
