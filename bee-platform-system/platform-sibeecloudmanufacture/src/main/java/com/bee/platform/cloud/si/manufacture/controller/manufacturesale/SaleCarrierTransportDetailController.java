package com.bee.platform.cloud.si.manufacture.controller.manufacturesale;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.BuyCarDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleBackAssayResultDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleCarDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleTransportDetailDTO;
import com.bee.platform.cloud.si.manufacture.rq.SaleBackAssayResultSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.SaleDiscountTranspoerDetailRq;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleBackAssayResultService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleCarrierTransportDetailService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

/**
 * <p>
 * 承运方运输详情表(销售) 前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "saleCarrierTransportDetail", tags = "销售-承运方运输车次详情相关接口")
@RequestMapping("/saleCarrierTransportDetail")
public class SaleCarrierTransportDetailController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    
    @Autowired
    private SaleBackAssayResultService saleBackAssayResultService;

    @Autowired
    private SaleCarrierTransportDetailService carrierTransportDetailService;
    
    
    @GetMapping(value = "/getAssayResult")
    @ApiOperation(value = "查看化验结果-查询反馈化验结果信息", notes = "查看化验结果-查询反馈化验结果信息")
    public ResponseResult<List<SaleBackAssayResultDTO>> getAssayResult(@RequestHeader("sysToken") String sysToken,
                                                          @RequestParam(value = "carrierTransportDetailId") String carrierTransportDetailId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleBackAssayResultService.getAssayResult(carrierTransportDetailId);
    }
	
	@PostMapping(value = "/saveAssayResult")
    @ApiOperation(value = "保存反馈化验结果", notes = "保存反馈化验结果")
    public ResponseResult saveAssayResult(@RequestHeader(value = "sysToken") String sysToken,
                                            @RequestBody @Valid SaleBackAssayResultSaveRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleBackAssayResultService.saveAssayResult(rq, userInfo);
    }

    @PostMapping(value="/saveTransportSection")
    @ApiOperation(value="保存承运商车次信息",notes="保存物流批次运输段信息")
    public ResponseResult<ResCodeEnum> saveTransportSection(HttpServletRequest request,
                                                            @RequestBody() SaleTransportDetailDTO transportDetailDTO){
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return carrierTransportDetailService.saveTransportDetailByTransport(transportDetailDTO, userInfo);
    }

    @GetMapping(value = "/getNotArrivalTransportDetail")
    @ApiOperation(value = "查询合同下未到货的车辆信息", notes = "查询合同下未到货的车辆信息")
    @ApiImplicitParam(name = "contractBusinessId",value = "合同业务id" , required = true)
    public ResponseResult<List<SaleTransportDetailDTO>> getNotArrivalTransportDetail(@RequestParam(value = "contractBusinessId") String contractBusinessId) {
        return carrierTransportDetailService.getNotArrivalTransportDetail(contractBusinessId);
    }

    @PostMapping(value = "/saveDiscountTransportDetail")
    @ApiOperation(value = "保存被折价的车辆单价信息", notes = "保存被折价的车辆单价信息")
    public ResponseResult saveDiscountTransportDetail(@RequestHeader(value = "sysToken") String sysToken,
                                          @RequestBody @Valid SaleDiscountTranspoerDetailRq rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return carrierTransportDetailService.saveDiscountTransportDetail(rq, userInfo);
    }
    
    /*************************************密云后台-销售合同-收货情况相关接口 *********************************************/
    @GetMapping(value = "/getCarList")
    @ApiOperation(value = "合同查看-收货情况-查看车辆信息", notes = "合同查看-收货情况-查看车辆信息")
    public ResponseResult<List<SaleCarDTO>> getCarList(@RequestHeader("sysToken") String sysToken,
    		                                          @RequestParam(value = "contractBusinessId") String contractBusinessId,
    		                                          Page page) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return carrierTransportDetailService.getCarList(contractBusinessId, userInfo, pagination);
    }
}

