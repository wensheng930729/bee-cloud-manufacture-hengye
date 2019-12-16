package com.bee.platform.cloud.si.manufacture.controller.manufacturebuy;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuySampleService;
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
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;


/**
 * <p>
 * 采购样品相关接口
 * </p>
 *
 * @author liliang123
 * @since 2019-09-23
 */
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/buySample")
@Api(value = "buySample", tags = "样品采购相关接口")
public class BuySampleController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    @Autowired
    private BuySampleService buySampleService;

    @PostMapping(value = "/purchaserReAssay")
    @ApiOperation(value = "采购采购商重新化验", notes = "采购采购商重新化验")
    public ResponseResult<ResCodeEnum> purchaserReAssay(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid PurchaserReAssayBuyRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buySampleService.purchaserReAssay(rq, userInfo);
    }


    @GetMapping(value = "/getSampleInfoList")
    @ApiOperation(value = "采购-获取样品信息列表", notes = "采购-获取样品信息列表(0-未确认，1-已确认)")
    public ResponseResult<BuySampleInfoDTO> getBuySampleInfoList(@RequestHeader("sysToken") String sysToken,
                                                                 @RequestParam(value = "confirmStatus") Integer confirmStatus,
                                                                 Page page) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return buySampleService.getBuySampleInfoList(userInfo, confirmStatus, pagination);
    }

    @GetMapping(value = "/getSampleInfo")
    @ApiOperation(value = "点击确认-获取样品详细信息", notes = "采购-获取样品详细信息")
    public ResponseResult<SampleResultDTO> getBuySampleInfo(@RequestHeader("sysToken") String sysToken,
                                                            @RequestParam(value = "sampleCode") String sampleCode) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buySampleService.getBuySampleInfo(sampleCode,userInfo);
    }

    @PostMapping(value = "/sure")
    @ApiOperation(value = "确认采样样品，出质检单", notes = "确认采样样品，出质检单")
    public ResponseResult sureBuySampleResult(@RequestHeader(value = "sysToken") String sysToken,
                                              @RequestBody @Valid BuySampleSureRQ sampleSureRQ) {
        // 参数验证
        if (ObjectUtils.isEmpty(sampleSureRQ)
                || StringUtils.isEmpty(sampleSureRQ.getSampleCode())
                || sampleSureRQ.getAssayResult() == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buySampleService.sureBuySampleResult(userInfo, sampleSureRQ);
    }

    @PostMapping(value = "/sendBack")
    @ApiOperation(value = "质检退回，重新化验", notes = "质检退回，重新化验")
    public ResponseResult sendBackBuySample(@RequestHeader(value = "sysToken") String sysToken,
                                            @RequestBody @Valid BuySampleSureRQ sampleSureRQ) {
        // 参数验证
        if (ObjectUtils.isEmpty(sampleSureRQ) || StringUtils.isEmpty(sampleSureRQ.getSampleCode())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buySampleService.sendBackBuySample(userInfo, sampleSureRQ);
    }

    @GetMapping(value = "/getCarSampleInfoList")
    @ApiOperation(value = "采购-获取车次样品信息列表", notes = "采购-获取车次样品信息列表")
    public ResponseResult<BuyCarSampleInfoDTO> getCarSampleInfoList(@RequestHeader("sysToken") String sysToken,
                                                                    @RequestParam(value = "confirmStatus") Integer confirmStatus,
                                                                    Page page) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return buySampleService.getCarSampleInfoList(userInfo, confirmStatus, pagination);
    }

    @GetMapping(value = "/getCarSampleInfo")
    @ApiOperation(value = "采购-获取车次样品详细信息", notes = "采购-获取车次样品详细信息（根据磅单id）")
    public ResponseResult<BuyCarSampleMsgDTO> getCarSampleInfo(@RequestHeader("sysToken") String sysToken,
                                                               @RequestParam(value = "machineId") String machineId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buySampleService.getCarSampleInfo(machineId, userInfo);
    }


    @PostMapping(value = "/sureCarSample")
    @ApiOperation(value = "车次货物确认", notes = "车次货物确认")
    public ResponseResult sureCarSampleResult(@RequestHeader(value = "sysToken") String sysToken,
                                              @RequestBody @Valid BuyCarSampleSureRQ carSampleSureRQ) {
        // 参数验证
        if (ObjectUtils.isEmpty(carSampleSureRQ)
                || StringUtils.isEmpty(carSampleSureRQ.getMachineId())
                || carSampleSureRQ.getAssayResult() == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buySampleService.sureCarSampleResult(userInfo, carSampleSureRQ);
    }

    @GetMapping(value = "/getUnqualifiedCarList")
    @ApiOperation(value = "合同查看-查看不合格车辆信息", notes = "合同查看-查看不合格车辆信息")
    public ResponseResult<List<BuyUnqualifiedCarDTO>> getUnqualifiedCarList(@RequestHeader(value = "sysToken") String sysToken,
    																		@RequestParam(value = "contractBusinessId") String contractBusinessId) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buySampleService.getUnqualifiedCarList(contractBusinessId, userInfo);
    }

    @PostMapping(value = "/saveSample")
    @ApiOperation(value = "取样-保存取样", notes = "取样-保存取样")
    public ResponseResult<String> saveSample(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid SaveSampleBuyRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buySampleService.saveSample(rq, userInfo);
    }

    @PostMapping(value = "/finishSample")
    @ApiOperation(value = "取样-完成取样", notes = "取样-完成取样")
    public ResponseResult<ResCodeEnum> finishSample(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid FinishSampleBuyRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buySampleService.finishSample(rq, userInfo);
    }

    /*************************************密云后台-采购合同-收货情况相关接口 *********************************************/
    @GetMapping(value = "/getCarList")
    @ApiOperation(value = "合同查看-收货情况-查看车辆信息", notes = "合同查看-收货情况-查看车辆信息")
    public ResponseResult<List<BuyCarDTO>> getCarList(@RequestHeader("sysToken") String sysToken,
    		                                          @RequestParam(value = "contractBusinessId") String contractBusinessId,
    		                                          Page page) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return buySampleService.getCarList(contractBusinessId, userInfo, pagination);
    }
}

