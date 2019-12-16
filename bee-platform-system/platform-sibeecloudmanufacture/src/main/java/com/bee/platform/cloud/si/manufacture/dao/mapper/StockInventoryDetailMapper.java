package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.StockInventoryDetail;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 库存盘点详细 Mapper 接口
 * </p>
 *
 * @author junyang.li123
 * @since 2019-11-25
 */
public interface StockInventoryDetailMapper extends BaseMapper<StockInventoryDetail> {

    /**
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 10:45 2019/11/27
     * @param list :
     * @return: void
     */
    void insertAll(@Param("data") List<StockInventoryDetail> list);

}
