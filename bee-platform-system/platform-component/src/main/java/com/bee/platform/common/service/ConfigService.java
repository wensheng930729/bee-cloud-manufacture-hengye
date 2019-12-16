package com.bee.platform.common.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.Config;

/**
 * <p>
 * 公共业务配置表 服务类
 * </p>
 *
 * @author zhigang.zhou
 * @since 2018-11-02
 */
public interface ConfigService extends IService<Config> {

	
	/**@author zhigang.zhou
	 * @Description 根据配置的key获取配置信息
	 * @param configKey 配置的键
	 * @date 2018年11月2日 下午3:40:53
	 */
	Config getConfigByconfigKey(String configKey);
	/**
	 * @notes: 从数据库中获得配置的数据
	 * @Author: junyang.li
	 * @Date: 13:53 2019/10/9
	 * @param key : 配置表键
	 * @param defaultValue : 默认值
	 * @param desc : 描述
	 * @return: java.lang.String
	 */
	String getConfValueInDB(String key, String defaultValue, String desc);
	/**@author zhigang.zhou
	 * @Description 根据配置的键获取该键对应的值（如果配置表中没有该键对应的记录就会插入一条初始化数据，否则只是查询数据表中的数据）
	 * @param key 键
	 * @param defaultValue 默认值
	 * @param desc 该配置的描述信息
	 * @date 2018年11月2日 下午3:37:20
	 */
	String getConfValue(String key, String defaultValue, String desc);	
	
	/**@author zhigang.zhou
	 * @Description 根据配置的键从数据库中获取该键对应的值（如果配置表中没有该键对应的记录就会插入一条初始化数据，否则只是查询数据表中的数据）
	 * @param key 键
	 * @param defaultValue 默认值
	 * @param desc 该配置的描述信息
	 * @date 2018年11月2日 下午3:37:20
	 */
	public String getConfValueFromDb(String key, String defaultValue, String desc);
}
