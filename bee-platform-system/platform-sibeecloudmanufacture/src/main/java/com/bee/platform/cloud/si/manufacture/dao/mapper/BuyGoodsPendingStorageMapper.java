package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.entity.BuyGoodsPendingStorage;

import java.math.BigDecimal;
import java.util.Map;

/**
 * @ClassName: BuyGoodsPendingStorageMapper
 * @Description: 采购待入库mapper
 * @Author: fei.sun
 * @Date: 2019/9/23 13:50
 * @Version: 1.0
 */
public interface BuyGoodsPendingStorageMapper extends BaseMapper<BuyGoodsPendingStorage> {

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
