package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.SaleLogisticsBatch;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.Date;

/**
 * <p>
 * 物流批次表(销售) Mapper 接口
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
public interface SaleLogisticsBatchMapper extends BaseMapper<SaleLogisticsBatch> {

    /**
     * 根据批次id查询最早出厂时间
     * @param batchId
     * @return
     */
    Date selectBatchDepartureTime(@Param("batchId") String batchId);

    /**
     * 根据批次id查询到厂货运方式
     * @param batchId
     * @return
     */
    String selectBatchTransportMode(@Param("batchId") String batchId);

}
