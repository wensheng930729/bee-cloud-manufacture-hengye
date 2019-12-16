package com.bee.platform.cloud.si.manufacture.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.ConfigReportFieldsService;
import com.bee.platform.cloud.si.manufacture.service.StorageInventoryService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyCarrierTransportService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyContractBasicService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleCarrierTransportService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.Map;


/**
 * <p>
 * 报表相关接口
 * </p>
 *@author
 *@date
 */
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/reportForm")
@Api(value = "reportForm", tags = "报表相关接口")
public class ReportFormController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    @Autowired
    private ConfigReportFieldsService reportFormService;
    @Autowired
    private BuyContractBasicService buyContractBasicService;
    @Autowired
    private BuyCarrierTransportService buyCarrierTransportService;
    @Autowired
    private SaleCarrierTransportService saleCarrierTransportService;

    @GetMapping("/getReportFormFields")
    @ApiOperation(value = "获取报表表头字段  businessType:1采购2销售3生产4进出厂【采购+销售】 " +
            "reportType:1现存明细表,2采购入库表,3原料日报表,4产成品入库,5成品出库,6生产质检,7进出质检,8采购,9销售,10产量分析,11合格率,12产量消耗分析,13物流-采购,14物流-销售")
    public ResponseResult<ReportFormFieldsTotalDTO> getReportFormFields(@RequestHeader("sysToken") String sysToken,
                                                                        String reportType,
                                                                        Integer productId,
                                                                        @RequestParam("businessType") Integer businessType) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return reportFormService.getReportFormFields(userInfo, reportType, productId, businessType);
    }

    @GetMapping("/getProductCategories")
    @ApiOperation(value = "获取当前工厂可用类别产品信息")
    public ResponseResult<List<ReportFormProductCategoryDTO>> getProductCategories(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return reportFormService.getProductCategories(userInfo);
    }

    @ApiOperation(value = "报表-质检报表")
    @GetMapping("/getQualityTestReportForm")
    public ResponseResult<ReportFormTestQualityTestTotalDTO> getQualityTestReportForm(@RequestHeader("sysToken") String sysToken,
                                                                                       @Valid ReportFormQualityTestRq rq, Page page) {
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return reportFormService.getQualityTestReportForm(userInfo, rq, pagination);
    }

    @GetMapping(value = "/getProducts")
    @ApiOperation(value = "获取可选产品", notes = "获取可选产品")
    public ResponseResult<List<ProductDTO>> getProducts(@RequestHeader(value = "sysToken") String sysToken) {
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractBasicService.getProducts(userInfo);
    }

    @ApiOperation(value = "报表-采购报表")
    @GetMapping("/getBuyReportForm")
    public ResponseResult<ReportFormBuyTotalDTO> getBuyReportForm(@RequestHeader("sysToken") String sysToken,
                                                                  @Valid ReportFormBuyRq rq, Page page) {
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return reportFormService.getBuyReportForm(userInfo, rq, pagination);
    }

    @GetMapping("/getPassRateReportForm")
    @ApiOperation(value = "报表-合格率报表")
    public ResponseResult<ReportFormDataInfoDTO> getPassRateReportForm(@RequestHeader("sysToken") String sysToken,
                                                                   @Valid ReportFormPassRateRq rq){
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return reportFormService.getPassRateReportForm(userInfo, rq);
    }


    @ApiOperation(value = "报表-销售报表")
    @GetMapping("/getSaleReportForm")
    public ResponseResult<ReportFormSaleTotalDTO> getSaleReportForm(@RequestHeader("sysToken") String sysToken,
                                                                    @Valid ReportFormSaleRq rq, Page page) {
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return reportFormService.getSaleReportForm(userInfo, rq, pagination);

    }


    @ApiOperation(value = "报表-库存-现存明细表")
    @GetMapping("/getExistingDeatilsReportForm")
    public ResponseResult<ReportFormExistingAllDTO> getExistingDeatilsReportForm(@RequestHeader("sysToken") String sysToken,
                        @Valid ReportFormExistingDeatilsRq rq, Page page) {
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return reportFormService.getExistingDeatilsReportForm(userInfo, rq, pagination);
    }



    @ApiOperation(value = "报表-库存-采购入库表")
    @GetMapping("/getBuySendStorageReportForm")
    public ResponseResult<ReportBuySendStorageAllDTO> getBuySendStorageReportForm(@RequestHeader("sysToken") String sysToken,
                        @Valid ReportFormExistingDeatilsRq rq, Page page){
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return reportFormService.getBuySendStorageReportForm(userInfo,rq,pagination);
    }


    @ApiOperation(value = "报表-库存-原料日报表")
    @GetMapping("/getRawMaterialReportForm")
    public ResponseResult<ReportRawMaterialAllDTO> getRawMaterialReportForm(@RequestHeader("sysToken") String sysToken,
                         @Valid ReportRawMaterialRq rq, Page page){
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return  reportFormService.getRawMaterialReportForm(userInfo,rq,pagination);
    }

    @ApiOperation(value = "报表-产成品入库报表")
    @GetMapping("/getProductWarehouseReportForm")
    public ResponseResult<ReportFormProductWarehouseTotalDTO> getProductWarehouseReportForm(@RequestHeader("sysToken") String sysToken,
                                                                                            @Valid ReportFormWarehouseRq rq, Page page) {
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return reportFormService.getProductWarehouseReportForm(userInfo, rq, pagination);
    }


    @ApiOperation(value = "报表-库存-成品出库表")
    @GetMapping("/getProductOutStorageReportForm")
    public ResponseResult<ReportProductOutStorageAllDTO> getProductOutStorageReportForm(@RequestHeader("sysToken") String sysToken,
                                                                                              @Valid ReportFormExistingDeatilsRq rq, Page page){
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return reportFormService.getProductOutStorageReportForm(userInfo,rq,pagination);
    }


    @GetMapping("/getYieldAnalysisReportForm")
    @ApiOperation(value = "报表-产量分析报表")
    public ResponseResult<ReportFormDataInfoDTO> getYieldAnalysisReportForm(@RequestHeader("sysToken") String sysToken,
                                                                            @Valid ReportFormYieldAnalysisRq rq, Page page){
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return reportFormService.getYieldAnalysisReportForm(userInfo, rq, pagination);
    }

    @GetMapping("/getBuyLogisticsReportForm")
    @ApiOperation(value = "报表-运输台账-采购物流报表")
    public ResponseResult<List<BuyTransportReportDTO>> getBuyLogisticsReportForm(@RequestHeader("sysToken") String sysToken,
                                                                            @Valid BuyCarrierTransportSearchRQ rq, Page page){
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return buyCarrierTransportService.getCarrierTransportBy(userInfo, rq, pagination);
    }

    @GetMapping("/getSaleLogisticsReportForm")
    @ApiOperation(value = "报表-运输台账-销售物流报表")
    public ResponseResult<List<SaleTransportReportDTO>> getSaleLogisticsReportForm(@RequestHeader("sysToken") String sysToken,
                                                                                 @Valid SaleCarrierTransportSearchRQ rq, Page page){
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return saleCarrierTransportService.getCarrierTransportBy(userInfo, rq, pagination);
    }


    @ApiOperation(value = "获取所有炉号")
    @GetMapping("/getFurnaces")
    public ResponseResult<List<ReportFurnacesDTO>> getFurnaces(@RequestHeader("sysToken") String sysToken){
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return reportFormService.getFurnaces(userInfo);
    }

    @ApiOperation(value = "报表-产量消耗分析表-表头字段(1-产量统计 2-消耗分析)")
    @GetMapping("/getOutputStatisticsReportFormFields")
    public ResponseResult<Map<String,Object>> getOutputStatisticsReportFormFields(@RequestHeader("sysToken") String sysToken,
                                                                                  @Valid ReportFormOutputStatisticsRq rq) {
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (rq.getType() == 1) {
            return reportFormService.getOutputStatisticsReportFormFields(userInfo,rq);
        } else {
            return reportFormService.getConsumptionAnalysisReportFormFields(userInfo,rq);
        }
    }

    @ApiOperation(value = "报表-产量消耗分析表-报表数据(1-产量统计 2-消耗分析)")
    @GetMapping("/getOutputStatisticsReportForm")
    public ResponseResult<ReportFormDataInfoDTO> getOutputStatisticsReportForm(@RequestHeader("sysToken") String sysToken,
                                                                                   @Valid ReportFormOutputStatisticsRq rq) {
        //用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (rq.getType() == 1) {
            return reportFormService.getOutputStatisticsReportForm(userInfo, rq);
        } else {
            return reportFormService.getConsumptionAnalysisReportForm(userInfo, rq);
        }
    }
}

