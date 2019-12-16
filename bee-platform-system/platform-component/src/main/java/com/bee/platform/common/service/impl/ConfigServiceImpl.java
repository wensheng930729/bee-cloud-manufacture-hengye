package com.bee.platform.common.service.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.dao.mapper.ConfigMapper;
import com.bee.platform.common.entity.Config;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;

import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;

/**
 * <p>
 * 公共业务配置表 服务实现类
 * </p>
 *
 * @author zhigang.zhou
 * @since 2018-11-02
 */
@Slf4j
@Service
public class ConfigServiceImpl extends ServiceImpl<ConfigMapper, Config> implements ConfigService {

	@Autowired
	private ConfigMapper configMapper;
	
	@Autowired
	private JedisService jedisService;

	/**@author zhigang.zhou
	 * @Description 根据配置的key获取配置信息
	 * @param configKey 配置的键
	 * @date 2018年11月2日 下午3:40:53
	 */
	@Override
	public Config getConfigByconfigKey(String configKey) {
		return StringUtils.isBlank(configKey) ? null
				: configMapper.selectOne(new Config().setConfigKey(configKey).setStatus("1"));
	}
	/**
	 * @notes: 从数据库中获得配置的数据
	 * @Author: junyang.li
	 * @Date: 13:53 2019/10/9
	 * @param key : 配置表键
	 * @param defaultValue : 默认值
	 * @param desc : 描述
	 * @return: java.lang.String
	 */
	@Override
	public String getConfValueInDB(String key, String defaultValue, String desc) {
		if(StringUtils.isBlank(key)) {
			return null;
		}
		Config config = configMapper.selectOne(new Config().setConfigKey(key.trim()));
		String status = "1";
		if(null == config) {
			Config c = new Config();
			configMapper.insert(c.setConfigKey(key.trim())
					.setConfigValue(defaultValue)
					.setConfigDesc(desc)
					.setStatus(status)
					.setCreateTime(new Date()));
			return c.getConfigValue();
		}
		if(!status.equals(config.getStatus())) {
			return null;
		}
		return config.getConfigValue();
	}
	/**@author zhigang.zhou
	 * @Description 根据配置的键获取该键对应的值（如果配置表中没有该键对应的记录就会插入一条初始化数据，否则只是查询数据表中的数据）
	 * @param key 键
	 * @param defaultValue 默认值
	 * @param desc 该配置的描述信息
	 * @date 2018年11月2日 下午3:37:20
	 */
	@Override
	public String getConfValue(String key, String defaultValue, String desc) {
		if(StringUtils.isBlank(key)) {
			return null;
		}
		// 获取配置的全局缓存开关
		String globaleConfigSwitch = getConfValueFromDb(ConstantsUtil.CMF_CONFIG_SWITCH, "1",
				"所有配置是否走缓存的全局开关:1启用缓存,0不走缓存");
		log.info("配置的全局缓存开关1启用缓存,0不走缓存,globaleConfigSwitch={}", globaleConfigSwitch);
		if (!"1".equals(globaleConfigSwitch)) {
			return getConfValueFromDb(key, defaultValue, desc);
		}
		try {
			// 从缓存中获取
			String cacheValue = jedisService.get(key);
			if(StringUtils.isNotBlank(cacheValue)) {
				return cacheValue;
			}
		} catch (Exception e) {
			log.error("从redis中获取配置异常,请检查redis连接");
		}
		// 缓存中没有就从数据库中查询
		Config config = configMapper.selectOne(new Config().setConfigKey(key.trim()));
		String status = "1";
		if(null == config) {
			Config c = new Config();
			configMapper.insert(c.setConfigKey(key.trim())
					.setConfigValue(defaultValue)
					.setConfigDesc(desc)
					.setStatus(status)
					.setCreateTime(new Date()));
			String configValue = c.getConfigValue();
			try {
				// 放到缓存中
				jedisService.set(key, configValue, 2 * 60 * 60);
			} catch (Exception e) {
				log.error("设置配置到redis异常,请检查redis连接");
			}
			return configValue;
		}
		if(!status.equals(config.getStatus())) {
			return null;
		}
		String confValue = config.getConfigValue();
		try {
			// 放到缓存中
			jedisService.set(key, confValue, 2 * 60 * 60);
		} catch (Exception e) {
			log.error("设置配置到redis异常,请检查redis连接");
		}
		return confValue;
	}

	/**@author zhigang.zhou
	 * @Description 根据配置的键从数据库中获取该键对应的值（如果配置表中没有该键对应的记录就会插入一条初始化数据，否则只是查询数据表中的数据）
	 * @param key 键
	 * @param defaultValue 默认值
	 * @param desc 该配置的描述信息
	 * @date 2018年11月2日 下午3:37:20
	 */
	@Override
	public String getConfValueFromDb(String key, String defaultValue, String desc) {
		if(StringUtils.isBlank(key)) {
			return null;
		}
		// 缓存中没有就从数据库中查询
		Config config = configMapper.selectOne(new Config().setConfigKey(key.trim()));
		String status = "1";
		if(null == config) {
			Config c = new Config();
			configMapper.insert(c.setConfigKey(key.trim())
					.setConfigValue(defaultValue)
					.setConfigDesc(desc)
					.setStatus(status)
					.setCreateTime(new Date()));
			String configValue = c.getConfigValue();
			try {
				// 放到缓存中
				jedisService.set(key, configValue, 2 * 60 * 60);
			} catch (Exception e) {
				log.error("设置配置到redis异常,请检查redis连接,此时走数据库不影响正常使用");
			}
			return configValue;
		}
		if(!status.equals(config.getStatus())) {
			return null;
		}
		String confValue = config.getConfigValue();
		try {
			// 放到缓存中
			jedisService.set(key, confValue, 2 * 60 * 60);
		} catch (Exception e) {
			log.error("设置配置到redis异常,请检查redis连接,此时走数据库不影响正常使用");
		}
		return confValue;
	}


}
