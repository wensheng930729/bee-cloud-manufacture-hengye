package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.SampleAssayResult;

import java.util.List;

import com.baomidou.mybatisplus.mapper.BaseMapper;

/**
 * <p>
 * 样品化验结果表 Mapper 接口
 * </p>
 *
 * @author liliang123
 * @since 2019-09-23
 */
public interface SampleAssayResultMapper extends BaseMapper<SampleAssayResult> {

	/**
	 * 软删除分析项
	 * @param sampleCode
	 */
	void deleteDateById(String sampleCode);

}
