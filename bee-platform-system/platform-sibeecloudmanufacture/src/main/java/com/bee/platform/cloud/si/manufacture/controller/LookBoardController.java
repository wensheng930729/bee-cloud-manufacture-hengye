package com.bee.platform.cloud.si.manufacture.controller;

import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.rq.LookBoardRQ;
import com.bee.platform.cloud.si.manufacture.rq.PurchasePaymentRQ;
import com.bee.platform.cloud.si.manufacture.service.LookBoardService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * 看板相关接口
 *
 * @author dell
 */
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/lookBoard")
@Api(value = "lookBoard", tags = "看板相关接口")
public class LookBoardController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    @Autowired
    private LookBoardService lookBoardService;

    @GetMapping("/getBuyUnfinishedFinance")
    @ApiOperation(value = "采购-获取未完成业务的财务情况信息")
    public ResponseResult<List<UnfinishedFinanceDTO>> getBuyUnfinishedFinance(@RequestHeader("sysToken") String sysToken,
                                                                              @RequestParam(value = "type") Integer type) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (type == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return lookBoardService.getBuyUnfinishedFinance(userInfo, type);
    }

    @GetMapping("/getSaleUnfinishedFinance")
    @ApiOperation(value = "销售-获取未完成业务的财务情况信息")
    public ResponseResult<List<UnfinishedFinanceDTO>> getSaleUnfinishedFinance(@RequestHeader("sysToken") String sysToken,
                                                                               @RequestParam(value = "type") Integer type) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (type == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return lookBoardService.getSaleUnfinishedFinance(userInfo, type);
    }

    @PostMapping("/getBuyUnfinishedGoods")
    @ApiOperation(value = "采购-获取未完成业务的财务到货信息")
    public ResponseResult<List<UnfinishedGoodsDTO>> getBuyUnfinishedGoods(@RequestHeader("sysToken") String sysToken,
                                                                          @RequestBody @Valid LookBoardRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getBuyUnfinishedGoods(rq, userInfo);
    }

    @PostMapping("/getSaleUnfinishedGoods")
    @ApiOperation(value = "销售-获取未完成业务的发货情况信息")
    public ResponseResult<List<UnfinishedGoodsDTO>> getSaleUnfinishedGoods(@RequestHeader("sysToken") String sysToken,
                                                                           @RequestBody @Valid LookBoardRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getSaleUnfinishedGoods(rq, userInfo);
    }

    @PostMapping("/getPurchaseMoneyRatio")
    @ApiOperation(value = "采购总额")
    ResponseResult<List<BuyPurchaseMoneyRatioDTO>> getPurchaseMoneyRatio(@RequestBody @Valid PurchasePaymentRQ rq, @RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getPurchaseMoneyRatio(rq, userInfo);
    }

    @PostMapping("/getPurchaseAmountRatio")
    @ApiOperation(value = "采购总数量")
    ResponseResult<List<BuyPurchaseAmountRatioDTO>> getPurchaseAmountRatio(@RequestBody @Valid PurchasePaymentRQ rq, @RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getPurchaseAmountRatio(rq, userInfo);
    }


    @PostMapping("/getPurchasePaymentRatio")
    @ApiOperation(value = "采购总付款金额")
    ResponseResult<List<BuyPurchaseMoneyRatioDTO>> getPurchasePaymentRatio(@RequestBody @Valid PurchasePaymentRQ rq, @RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getPurchasePaymentRatio(rq, userInfo);
    }

    @PostMapping("/getPurchasePassRatio")
    @ApiOperation(value = "采购合格率")
    ResponseResult<List<BuyPurchasePassRatioDTO>> getPurchasePassRatio(@RequestBody @Valid PurchasePaymentRQ rq, @RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getPurchasePassRatio(rq, userInfo);
    }

    @PostMapping("/getSaleMoneyRatio")
    @ApiOperation(value = "销售总额")
    ResponseResult<List<SaleMoneyRatioDTO>> getSaleMoneyRatio(@RequestBody @Valid PurchasePaymentRQ rq, @RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getSaleMoneyRatio(rq, userInfo);
    }

    @PostMapping("/getSaleAmountRatio")
    @ApiOperation(value = "销售总数量")
    ResponseResult<List<SaleAmountRatioDTO>> getSaleAmountRatio(@RequestBody @Valid PurchasePaymentRQ rq, @RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getSaleAmountRatio(rq, userInfo);
    }

    @PostMapping("/getSaleMoneyBackRatio")
    @ApiOperation(value = "销售回款")
    ResponseResult<List<SaleMoneyRatioDTO>> getSaleMoneyBackRatio(@RequestBody @Valid PurchasePaymentRQ rq, @RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getSaleMoneyBackRatio(rq, userInfo);
    }

    @PostMapping("/getSalePassRatio")
    @ApiOperation(value = "销售合格率")
    ResponseResult<List<SalePassRatioDTO>> getSalePassRatio(@RequestBody @Valid PurchasePaymentRQ rq, @RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getSalePassRatio(rq, userInfo);
    }

    @GetMapping("/getDataScreen")
    @ApiOperation(value = "查询BI数据总览")
    ResponseResult<DataScreenDTO> getDataScreen(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardService.getDataScreen(userInfo);
    }
}
