package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.entity.SaleCarrierTransportDetail;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 承运方运输详情表(销售) Mapper 接口
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
public interface SaleCarrierTransportDetailMapper extends BaseMapper<SaleCarrierTransportDetail> {

    /**
     * 查询批次中第一段的汽运车辆信息
     * @param batchId
     * @return
     */
    List<SaleCarrierTransportDetail> getFirstSectionCarInfo(@Param("batchId") String batchId);

    /**
     * 查询阶段下的运量总和
     * @param transportSectionId
     * @return
     */
    BigDecimal selectVolumeByTransportSection(@Param("transportSectionId") String transportSectionId);

    /**
     * 查询合同下的到货总数量
     * @param contractBusinessId
     * @return
     */
    BigDecimal selectVolumeByContract(@Param("contractBusinessId") String contractBusinessId);

    /**
     * 查询批次下的到厂车辆信息
     * @param batchId
     * @return
     */
    List<SaleCarrierTransportDetail> getToFactoryCarInfo(@Param("batchId") String batchId);

    /**
     * 查询合同下未到货的到厂车辆信息
     * @param batchId
     * @return
     */
    List<SaleCarrierTransportDetail> getNotArrivalToFactoryCarInfo(@Param("contractBusinessId") String contractBusinessId);

    /**
     * 查询合同下收货情况
     * @param contractBusinessId
     * @param pagination 
     * @return
     */
	List<SaleCarrierTransportDetail> getCarList(@Param("contractBusinessId") String contractBusinessId, Pagination pagination);

}
