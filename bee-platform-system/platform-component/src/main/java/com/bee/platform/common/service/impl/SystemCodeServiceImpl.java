package com.bee.platform.common.service.impl;

import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.dao.mapper.SystemCodeMapper;
import com.bee.platform.common.entity.SystemCode;
import com.bee.platform.common.enums.ConfigKeyEnum;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.SystemCodeService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * <p>
 * 系统码表 服务实现类
 * </p>
 *
 * @author zhigang.zhou
 * @since 2018-11-02
 */
@Slf4j
@Service
public class SystemCodeServiceImpl extends ServiceImpl<SystemCodeMapper, SystemCode> implements SystemCodeService {

	@Autowired
	private SystemCodeMapper systemCodeMapper;
	
	@Autowired
    private RedisTemplate<String, String> redisTemplate;
	
	@Autowired
	private ConfigService configService;

	/**
	 * @author zhigang.zhou
	 * @Description 获取码表信息数据的接口
	 * @param redisKey   缓存的键
	 * @param codeTypeId 组ID
	 * @date 2018年11月2日 下午4:13:42
	 */
	@Override
	public List<SystemCode> getCacheSysCodeInfo(String redisKey, String codeTypeId) {
		// 获取缓存的开关
		String isCacheOpen = configService.getConfValue(ConfigKeyEnum.IS_ENABLESYSCODECACHE_KEY.codeValue, "1",
				ConfigKeyEnum.IS_ENABLESYSCODECACHE_KEY.desc);
		log.info("redisKey={}, codeTypeId={}, isCacheOpen={}", redisKey, codeTypeId, isCacheOpen);
		// 当缓存开关处于关闭状态时，从数据库中查询
		if (!"1".equals(isCacheOpen)) {
			log.info("缓存开关处于关闭状态,redisKey={}, codeTypeId={}, isCacheOpen={}", redisKey, codeTypeId, isCacheOpen);
			List<SystemCode> list = systemCodeMapper
					.selectList(new EntityWrapper<SystemCode>()
							.eq("sys_group_id", codeTypeId)
							.eq("status", "1"));
			if (null == list || list.size() < 1) {
				return null;
			}
			if(StringUtils.isNotBlank(redisKey)) {
				// 将数据库的数据再缓存到缓存中
				String systemCodeResJsonStr = JSON.toJSONString(list);
				// 缓存两小时
				redisTemplate.opsForValue().set(redisKey, systemCodeResJsonStr);
				redisTemplate.expire(redisKey, 60 * 60 * 2, TimeUnit.SECONDS);
			}
			return list;
		}
		
		// 当缓存开关开启时
		if(StringUtils.isNotBlank(redisKey)) {
			String systemCodeJsonStr = redisTemplate.opsForValue().get(redisKey);
			if(StringUtils.isNotBlank(systemCodeJsonStr)) {
				List<SystemCode> resList = null;
				try {
					resList = JSON.parseArray(systemCodeJsonStr, SystemCode.class);
				} catch (Exception e) {
					log.error("解析缓存数据异常从数据库中查询,redisKey={}, codeTypeId={}, isCacheOpen={},解析的缓存数据systemCodeJsonStr={}",
							redisKey, codeTypeId, isCacheOpen, systemCodeJsonStr, e);
					// 如果解析抛出异常，就从数据库中查询并放进缓存中
					return getSysCodeByDbIntoCache(redisKey, codeTypeId);
				}
				// 缓存中有就直接返回缓存的数据
				if(null != resList && resList.size() > 0) {
					log.info("缓存的键不为空，缓存中数据不为空，直接返回缓存中的数据,redisKey={}, codeTypeId={}, isCacheOpen={}", redisKey,
							codeTypeId, isCacheOpen);
					return resList;
				}
				return getSysCodeByDbIntoCache(redisKey, codeTypeId);
			}
			// 缓存的键不为空，缓存中数据为空，则从数据库中查询，并放到缓存中
			log.info("缓存的键不为空，缓存中数据为空，则从数据库中查询，并放到缓存中,redisKey={}, codeTypeId={}, isCacheOpen={}", redisKey, codeTypeId,
					isCacheOpen);
			return getSysCodeByDbIntoCache(redisKey, codeTypeId);
		}
		log.info("缓存的键为空，直接从数据库中查询,redisKey={}, codeTypeId={}, isCacheOpen={}", redisKey, codeTypeId, isCacheOpen);
		// 缓存的键为空，直接从数据库中查询
		List<SystemCode> list = systemCodeMapper
				.selectList(new EntityWrapper<SystemCode>()
						.eq("sys_group_id", codeTypeId)
						.eq("status", "1"));
		if (null == list || list.size() < 1) {
			return null;
		}
		return list;
	}

	/**
	 *  从数据库中查询码表信息并放进缓存中
	 */
	private List<SystemCode> getSysCodeByDbIntoCache(String redisKey, String codeTypeId) {
		// 从数据库中查询
		List<SystemCode> list = systemCodeMapper
				.selectList(new EntityWrapper<SystemCode>()
						.eq("sys_group_id", codeTypeId)
						.eq("status", "1"));
		if (null == list || list.size() < 1) {
			return null;
		}
		// 将数据库的数据再缓存到缓存中
		String systemCodeResJsonStr = JSON.toJSONString(list);
		redisTemplate.opsForValue().set(redisKey, systemCodeResJsonStr);
		// 缓存两小时
		redisTemplate.expire(redisKey, 60 * 60 * 2, TimeUnit.SECONDS);
		return list;
	}
	
	
	
}
