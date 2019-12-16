package com.bee.platform.cloud.si.manufacture.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.ConfigReportFields;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.rq.ReportFormBuyRq;
import com.bee.platform.cloud.si.manufacture.rq.ReportFormExistingDeatilsRq;
import com.bee.platform.cloud.si.manufacture.rq.ReportFormPassRateRq;
import com.bee.platform.cloud.si.manufacture.rq.ReportFormQualityTestRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 报表字段配置表 服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-10-18
 */
public interface ConfigReportFieldsService extends IService<ConfigReportFields> {
    /**
     * 根据合同类型和产品id获取报表字段
     * @param userInfo
     * @param reportType
     * @param productId
     * @param businessType
     * @return
     */
    ResponseResult<ReportFormFieldsTotalDTO> getReportFormFields(AuthPlatformUserInfo userInfo, String reportType, Integer productId, Integer businessType);

    /**
     * 当前工厂可用类别产品信息
     * @param userInfo
     * @return List<ReportFormProductCategoryDTO>
     */
    ResponseResult<List<ReportFormProductCategoryDTO>> getProductCategories(AuthPlatformUserInfo userInfo);

    /**
     * 质检报表
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    ResponseResult<ReportFormTestQualityTestTotalDTO> getQualityTestReportForm(AuthPlatformUserInfo userInfo, ReportFormQualityTestRq rq, Pagination pagination);

    /**
     * 采购报表
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    ResponseResult<ReportFormBuyTotalDTO> getBuyReportForm(AuthPlatformUserInfo userInfo, ReportFormBuyRq rq, Pagination pagination);

    /**
     * 销售报表
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    ResponseResult<ReportFormSaleTotalDTO> getSaleReportForm(AuthPlatformUserInfo userInfo, ReportFormSaleRq rq, Pagination pagination);

    /**
     * 报表-产成品入库报表
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    ResponseResult<ReportFormProductWarehouseTotalDTO> getProductWarehouseReportForm(AuthPlatformUserInfo userInfo, ReportFormWarehouseRq rq, Pagination pagination);

    /**
     * 合格率报表
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<ReportFormDataInfoDTO> getPassRateReportForm(AuthPlatformUserInfo userInfo, ReportFormPassRateRq rq);


    /**
     * 报表-现存明细
     * @author chenxm66777123
     * @Date 2019/10/21 10:50
     * @version 1.0.0
     */
    ResponseResult<ReportFormExistingAllDTO> getExistingDeatilsReportForm(AuthPlatformUserInfo userInfo,ReportFormExistingDeatilsRq rq, Pagination pagination);

    /**
     * 报表-采购入库
     * @author chenxm66777123
     * @Date 2019/10/21 10:50
     * @version 1.0.0
     */
    ResponseResult<ReportBuySendStorageAllDTO> getBuySendStorageReportForm(AuthPlatformUserInfo userInfo,ReportFormExistingDeatilsRq rq, Pagination pagination);



    /**
     * 报表-原材料日报
     * @author chenxm66777123
     * @Date 2019/10/22 10:22
     * @version 1.0.0
     */
    ResponseResult<ReportRawMaterialAllDTO> getRawMaterialReportForm(AuthPlatformUserInfo userInfo,ReportRawMaterialRq rq, Pagination pagination);

    /**
     * 产量分析报表
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    ResponseResult<ReportFormDataInfoDTO> getYieldAnalysisReportForm(AuthPlatformUserInfo userInfo, ReportFormYieldAnalysisRq rq, Pagination pagination);



    /**
     * 报表-库存-成品出库表
     * @author chenxm66777123
     * @Date 2019/10/22 10:22
     * @version 1.0.0
     */
    ResponseResult<ReportProductOutStorageAllDTO> getProductOutStorageReportForm(AuthPlatformUserInfo userInfo,ReportFormExistingDeatilsRq rq, Pagination pagination);

    /**
     * 炉号下拉框
     * @param userInfo
     * @return
     */
    ResponseResult<List<ReportFurnacesDTO>> getFurnaces(AuthPlatformUserInfo userInfo);

    /**
     * 报表-生产产量统计报表表头字段
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Map<String, Object>> getOutputStatisticsReportFormFields(AuthPlatformUserInfo userInfo,ReportFormOutputStatisticsRq rq);

    /**
     * 生产产量统计报表
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<ReportFormDataInfoDTO> getOutputStatisticsReportForm(AuthPlatformUserInfo userInfo, ReportFormOutputStatisticsRq rq);

    /**
     * 报表-生产消耗分析报表表头字段
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<Map<String, Object>> getConsumptionAnalysisReportFormFields(AuthPlatformUserInfo userInfo,ReportFormOutputStatisticsRq rq);

    /**
     * 消耗分析统计报表
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<ReportFormDataInfoDTO> getConsumptionAnalysisReportForm(AuthPlatformUserInfo userInfo, ReportFormOutputStatisticsRq rq);

}
