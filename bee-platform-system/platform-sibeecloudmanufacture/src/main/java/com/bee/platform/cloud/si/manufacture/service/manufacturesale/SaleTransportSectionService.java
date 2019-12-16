package com.bee.platform.cloud.si.manufacture.service.manufacturesale;

import com.bee.platform.cloud.si.manufacture.dto.SaleTransportSectionDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleLogisticsBatch;
import com.bee.platform.cloud.si.manufacture.entity.SaleTransportSection;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.SaveTransportSectionRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 物流批次运输段表(销售) 服务类
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
public interface SaleTransportSectionService extends IService<SaleTransportSection> {

    /**
     * 根据批次id查询运输段信息
     * @param batchId
     * @return
     */
    List<SaleTransportSectionDTO> getTransportSectionInfo(String batchId);

    /**
     * 保存运输段信息，返回运输段的运量
     * @param transportSectionDTO
     * @param userInfo
     * @return
     */
    BigDecimal saveTransportSectionInfo(SaleTransportSectionDTO transportSectionDTO, AuthPlatformUserInfo userInfo);

    /**
     * 单独保存运输段信息
     * @param transportSectionDTO
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> saveTransportSection(SaleTransportSectionDTO transportSectionDTO, AuthPlatformUserInfo userInfo);

    /**
     * 新增物流批次运输段信息
     * @param transportSectionDTO
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> saveNewTransportSection(SaleTransportSectionDTO transportSectionDTO, AuthPlatformUserInfo userInfo);

    /**
     * 更新批次的运量和合同的在途量
     * @param contractBusinessId
     * @param batchId
     * @param freightVolume
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> updateContractFreightVolume(String contractBusinessId, String batchId,
                                                            BigDecimal freightVolume, AuthPlatformUserInfo userInfo);

    /**
     * 更新合同的已收货量
     * @param contractBusinessId
     * @param volume
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> updateContractReceivedVolume(String contractBusinessId, BigDecimal volume,
                                                            AuthPlatformUserInfo userInfo);

    /**
     * 根据批次id单独查询运输段信息
     * @param batchId
     * @return
     */
    List<SaleTransportSectionDTO> getTransportSectionInfoByBatchId(String batchId);

    /**
     * 保存运输段信息
     * @param transportSectionRq
     * @param logisticsBatch
     * @param userInfo
     * @return
     */
    void saveTransportSection(SaveTransportSectionRq transportSectionRq, SaleLogisticsBatch logisticsBatch, AuthPlatformUserInfo userInfo);

    /**
     * 根据运输段id查询运输段信息(全部承运商)
     * @param transportSectionId
     * @return
     */
    ResponseResult<SaleTransportSectionDTO> getTransportSectionAllDetail(String transportSectionId);

    /**
     * 根据运输段id单独查询运输段详细信息
     * @param transportSectionId
     * @return
     */
    ResponseResult<SaleTransportSectionDTO> getTransportSectionById(String transportSectionId);

}
