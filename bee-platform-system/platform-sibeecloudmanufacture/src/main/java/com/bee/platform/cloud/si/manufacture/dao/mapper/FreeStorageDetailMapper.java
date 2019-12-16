package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.entity.FreeStorageDetail;

import java.math.BigDecimal;
import java.util.Map;

/**
 * @ClassName: FreeStorageDetailMapper
 * @Description: 自由入库Mapper
 * @Author: fei.sun
 * @Date: 2019/9/26 13:55
 * @Version: 1.0
 */
public interface FreeStorageDetailMapper extends BaseMapper<FreeStorageDetail> {

    /**
     * @Description 获取今日入库重量
     * @author chenxm66777123
     * @Date 2019/10/22 14:41
     * @version 1.0.0
     */
    BigDecimal getTodayIntoStock(Map<String,Object> map);


    /**
     * @Description 获取开始日期之后的当日入库量
     * @author chenxm66777123
     * @Date 2019/10/22 15:09
     * @version 1.0.0
     */
    BigDecimal getTodayIntoStockAfter(Map<String,Object> map);
}
