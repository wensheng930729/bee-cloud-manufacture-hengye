package com.bee.platform.cloud.si.manufacture.service.manufacturesale;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.SaleLogisticsBatch;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.SaveLogisticsRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 物流批次表(销售) 服务类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
public interface SaleLogisticsBatchService extends IService<SaleLogisticsBatch> {

    /**
     * 根据合同业务id查询物流信息
     * @param contractBusinessId
     * @return
     */
    SaleLogisticsInfoDTO getLogisticsBatchList(String contractBusinessId);

    /**
     * 查询新增批次合同信息
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    ResponseResult<SaleLogisticsBatchDTO> getLogisticsContractInfo(String contractBusinessId, AuthPlatformUserInfo userInfo);

    /**
     * 查询合同批次阶段信息
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    ResponseResult<SaleNewLogisticsDTO> getLogisticsContractDetail(String contractBusinessId, AuthPlatformUserInfo userInfo);

    /**
     * 确认物流批次信息
     * @param batchId
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> saveLogisticsBatchInfo(String batchId, AuthPlatformUserInfo userInfo);

    /**
     * 查询合同详情中的物流批次信息
     * @param contractBusinessId
     * @return
     */
    ResponseResult<List<SaleContractBatchDTO>> getContractBatchInfo(String contractBusinessId);

    /**
     * 运输合同列表
     * @param keyword
     * @param purchaserMode
     * @param userInfo
     * @param pagination
     * @return
     */
    ResponseResult<List<SaleContractListContentDTO>> getLogisticsContractList(String keyword, Integer purchaserMode,
                                                                              Pagination pagination, AuthPlatformUserInfo userInfo);

    /**
     * 根据合同业务id查询物流批次阶段信息
     * @param contractBusinessId
     * @return
     */
    SaleLogisticsInfoDTO getLogisticsBatchSection(String contractBusinessId);

    /**
     * 新增物流批次阶段信息
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<String> saveLogisticsSectionInfo(SaveLogisticsRq rq, AuthPlatformUserInfo userInfo);

}
