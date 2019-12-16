package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.StockInventoryListParam;
import com.bee.platform.cloud.si.manufacture.entity.StockInventory;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 库存盘点主表 Mapper 接口
 * </p>
 *
 * @author junyang.li123
 * @since 2019-11-25
 */
public interface StockInventoryMapper extends BaseMapper<StockInventory> {

    /**
     * @notes: 统计当天的判断订单数量
     * @Author: junyang.li
     * @Date: 15:06 2019/11/26
     * @return: int
     */
    int countCurDateByOrder();
    /**
     * @notes: 盘点单列表查询 暂时只返回盘点单编号 和 创建时间
     * @Author: junyang.li
     * @Date: 14:49 2019/11/27
     * @param pagination : 分页对象
     * @param param : 查询参数
     * @return: java.util.List<com.bee.platform.cloud.si.manufacture.entity.StockInventory>
     */
    List<StockInventory> getInventoryInfoList(Pagination pagination,@Param("param") StockInventoryListParam param);
}
