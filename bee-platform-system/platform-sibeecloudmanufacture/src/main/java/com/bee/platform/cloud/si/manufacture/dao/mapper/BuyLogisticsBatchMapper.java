package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ContractLogisticInfoDTO;
import com.bee.platform.cloud.si.manufacture.dto.LogisticsContractListContentDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuyLogisticsBatch;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * 物流批次表(采购) Mapper 接口
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
public interface BuyLogisticsBatchMapper extends BaseMapper<BuyLogisticsBatch> {

    /**
     * 根据批次id查询最早出厂时间
     * @param batchId
     * @return
     */
    Date selectBatchDepartureTime(@Param("batchId") String batchId);

    /**
     * 根据批次id查询最早称重时间
     * @param batchId
     * @return
     */
    Date selectBatchWeightTime(@Param("batchId") String batchId);

    /**
     * 根据批次id查询到厂货运方式
     * @param batchId
     * @return
     */
    String selectBatchTransportMode(@Param("batchId") String batchId);

    /**
     * 查询采购销售中已完成的合同信息
     * @param orgId
     * @param pagination
     * @return
     */
    List<ContractLogisticInfoDTO> getContractLogisticInfo(Integer orgId, Pagination pagination);

    /**
     * 根据采购合同业务id查询最早出厂时间
     * @param contractBusinessId
     * @return
     */
    Date selectBuyContractDepartureTime(@Param("contractBusinessId") String contractBusinessId);

    /**
     * 根据采购合同业务id查询到货时间
     * @param contractBusinessId
     * @return
     */
    Date selectBuyContractArrivalTime(@Param("contractBusinessId") String contractBusinessId);

    /**
     * 根据销售合同业务id查询最早出厂时间
     * @param contractBusinessId
     * @return
     */
    Date selectSaleContractDepartureTime(@Param("contractBusinessId") String contractBusinessId);

    /**
     * 根据销售合同业务id查询到货时间
     * @param contractBusinessId
     * @return
     */
    Date selectSaleContractArrivalTime(@Param("contractBusinessId") String contractBusinessId);

    /**
     * 分页查询物流订单列表
     * @param params
     * @param pagination
     * @return
     */
    List<LogisticsContractListContentDTO> getAllLogisticsList(Map<String, Object> params, Pagination pagination);

}
