package com.bee.platform.common.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.SystemCode;

import java.util.List;

/**
 * <p>
 * 系统码表 服务类
 * </p>
 *
 * @author zhigang.zhou
 * @since 2018-11-02
 */
public interface SystemCodeService extends IService<SystemCode> {

	/**
	 * @author zhigang.zhou
	 * @Description 获取码表信息数据的接口
	 * @param redisKey   缓存的键
	 * @param codeTypeId 组ID
	 * @date 2018年11月2日 下午4:13:42
	 */
	List<SystemCode> getCacheSysCodeInfo(String redisKey, String codeTypeId);

}
