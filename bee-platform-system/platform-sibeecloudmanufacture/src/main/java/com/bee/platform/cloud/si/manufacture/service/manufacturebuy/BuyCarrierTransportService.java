package com.bee.platform.cloud.si.manufacture.service.manufacturebuy;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.BuyCarrierInfoDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyCarrierTransportDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyTransportReportDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuyCarrierTransport;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.entity.BuyContractBasic;
import com.bee.platform.cloud.si.manufacture.entity.BuyLogisticsBatch;
import com.bee.platform.cloud.si.manufacture.rq.BuyCarrierTransportSearchRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 承运方运输表(采购) 服务类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
public interface BuyCarrierTransportService extends IService<BuyCarrierTransport> {

    /**
     * 保存承运方运输段信息
     * @param carrierTransportDTO
     * @param userInfo
     */
    BigDecimal saveCarrierTransportInfo(BuyCarrierTransportDTO carrierTransportDTO, AuthPlatformUserInfo userInfo);

    /**
     * 根据运输段id查询运输段承运方信息
     * @param transportSectionId
     * @return
     */
    List<BuyCarrierTransportDTO> getCarrierTransportByBatch(String transportSectionId);

    /**
     * 根据运输段id查询到厂运输段承运方信息
     * @param transportSectionId
     * @return
     */
    List<BuyCarrierTransportDTO> getToFactoryCarrierTransport(String transportSectionId);

    /**
     * 单独保存运输段承运方信息
     * @param carrierTransportDTO
     * @param userInfo
     */
    ResponseResult<ResCodeEnum> saveCarrierTransport(BuyCarrierTransportDTO carrierTransportDTO, AuthPlatformUserInfo userInfo);

    /**
     * 将批次中的到厂车辆推送至磅房
     * @param contractBasic
     * @param logisticsBatch
     * @param userInfo
     */
    void pushCarInfoToWeight(BuyContractBasic contractBasic, BuyLogisticsBatch logisticsBatch, AuthPlatformUserInfo userInfo);

    /**
     * 根据承运方查询运输段承运方信息
     * @param pagination
     * @param userInfo
     * @return
     */
    ResponseResult<List<BuyCarrierTransportDTO>> getCarrierTransportByCarrier(Pagination pagination, AuthPlatformUserInfo userInfo);

    /**
     * 根据承运方id查询承运方相关运输段信息
     * @param carrierTransportId
     * @return
     */
    ResponseResult<BuyCarrierInfoDTO> getCarrierInfoByTransportId(String carrierTransportId);

    /**
     * 承运方保存运输段承运方信息
     * @param carrierTransportDTO
     * @param userInfo
     */
    ResponseResult<ResCodeEnum> saveCarrierTransportSection(BuyCarrierTransportDTO carrierTransportDTO, AuthPlatformUserInfo userInfo);

    /**
     * 根据条件查采购运输台账
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    ResponseResult<List<BuyTransportReportDTO>> getCarrierTransportBy(AuthPlatformUserInfo userInfo,
                                                                      BuyCarrierTransportSearchRQ rq, Pagination pagination);
}
