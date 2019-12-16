package com.bee.platform.common.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.common.entity.Region;
import com.bee.platform.common.entity.ResponseResult;

import java.util.Map;

/**
 * <p>
 * 全国地区表 Mapper 接口
 * </p>
 *
 * @author team123
 * @since 2018-12-24
 */
public interface RegionMapper extends BaseMapper<Region> {
    ResponseResult<Map<String,Object>> findAllRegionById(Integer id);

}
