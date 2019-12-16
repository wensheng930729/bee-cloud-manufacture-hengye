package com.bee.platform.cloud.si.manufacture.service.manufacturebuy;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.BuyLogisticsBatch;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.LogisticsContractListSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.SaveLogisticsRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;

import java.util.List;

/**
 * <p>
 * 物流批次表(采购) 服务类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
public interface BuyLogisticsBatchService extends IService<BuyLogisticsBatch> {

    /**
     * 根据合同业务id查询物流信息
     * @param contractBusinessId
     * @return
     */
    BuyLogisticsInfoDTO getLogisticsBatchList(String contractBusinessId);

    /**
     * 查询新增批次合同信息
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    ResponseResult<BuyLogisticsBatchDTO> getLogisticsContractInfo(String contractBusinessId, AuthPlatformUserInfo userInfo);

    /**
     * 查询合同批次阶段信息
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    ResponseResult<BuyNewLogisticsDTO> getLogisticsContractDetail(String contractBusinessId, AuthPlatformUserInfo userInfo);

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
    ResponseResult<List<BuyContractBatchDTO>> getContractBatchInfo(String contractBusinessId);

    /**
     * 运输合同列表
     * @param keyword
     * @param purchaserMode
     * @param userInfo
     * @param pagination
     * @return
     */
    ResponseResult<List<BuyContractListContentDTO>> getLogisticsContractList(String keyword, Integer purchaserMode,
                                                                Pagination pagination, AuthPlatformUserInfo userInfo);

    /**
     * 物流人员查询物流记录
     * @param pagination
     * @param userInfo
     * @return
     */
    ResponseResult<List<ContractLogisticInfoDTO>> getContractLogisticInfoList(Pagination pagination, AuthPlatformUserInfo userInfo);

    /**
     * 分页查询物流订单列表
     * @param rq
     * @param pagination
     * @param userInfo
     * @return
     */
    ResponseResult<List<LogisticsContractListContentDTO>> getAllLogisticsContractList(LogisticsContractListSearchRQ rq,
                                                                                      Pagination pagination, AuthPlatformUserInfo userInfo);

    /**
     * 根据合同业务id查询物流批次阶段信息
     * @param contractBusinessId
     * @return
     */
    BuyLogisticsInfoDTO getLogisticsBatchSection(String contractBusinessId);

    /**
     * 新增物流批次阶段信息
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<String> saveLogisticsSectionInfo(SaveLogisticsRq rq, AuthPlatformUserInfo userInfo);

    /**
     * 查询合同的第一个批次id
     * @param contractBusinessId
     * @return
     */
    String getContractLogisticsBatchId(String contractBusinessId);

}
