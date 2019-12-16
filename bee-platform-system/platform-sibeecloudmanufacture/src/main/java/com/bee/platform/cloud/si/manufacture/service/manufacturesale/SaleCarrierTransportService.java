package com.bee.platform.cloud.si.manufacture.service.manufacturesale;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.SaleCarrierTransportDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleTransportReportDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleCarrierTransport;
import com.bee.platform.cloud.si.manufacture.rq.SaleCarrierTransportSearchRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 运输段承运方表(销售) 服务类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
public interface SaleCarrierTransportService extends IService<SaleCarrierTransport> {

    /**
     * 保存承运方运输段信息
     * @param carrierTransportDTO
     * @param transportSection
     * @param userInfo
     */
    BigDecimal saveCarrierTransportInfo(SaleCarrierTransportDTO carrierTransportDTO, Integer transportSection, AuthPlatformUserInfo userInfo);

    /**
     * 根据运输段id查询运输段承运方信息
     * @param transportSectionId
     * @return
     */
    List<SaleCarrierTransportDTO> getCarrierTransportByBatch(String transportSectionId);

    /**
     * 单独保存运输段承运方信息
     * @param carrierTransportDTO
     * @param userInfo
     */
    ResponseResult<ResCodeEnum> saveCarrierTransport(SaleCarrierTransportDTO carrierTransportDTO, AuthPlatformUserInfo userInfo);

    /**
     * 根据条件查销售运输台账
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    ResponseResult<List<SaleTransportReportDTO>> getCarrierTransportBy(AuthPlatformUserInfo userInfo,
                                                                       SaleCarrierTransportSearchRQ rq, Pagination pagination);

}
