package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.ProductionOutStorageDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.math.BigDecimal;
import java.util.Map;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author MP123
 * @since 2019-09-27
 */
public interface ProductionOutStorageDetailMapper extends BaseMapper<ProductionOutStorageDetail> {


    BigDecimal getTodayUsed(Map<String,Object> map);

    BigDecimal getTodayUsedAfter(Map<String,Object> map);

}
