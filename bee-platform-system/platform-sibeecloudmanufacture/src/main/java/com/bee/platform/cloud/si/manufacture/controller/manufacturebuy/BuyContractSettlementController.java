package com.bee.platform.cloud.si.manufacture.controller.manufacturebuy;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractListDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractSettleInfoDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractSettleListDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractSettlePopupDTO;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyContractSettlementService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "buyContractSettlement", tags = "采购-合同结算相关接口")
@RequestMapping("/buyContractSettlement")
public class BuyContractSettlementController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    @Autowired
    private BuyContractSettlementService buyContractSettlementService;

    @GetMapping(value="/getSettleListBuyContract")
    @ApiOperation(value="采购合同供销结算列表 0未结算 1已结算",notes="采购合同供销结算列表 0未结算 1已结算")
    public ResponseResult<BuyContractListDTO> addSettleBuyContract(@RequestHeader(value = "sysToken")String sysToken,
                                                                   @RequestParam(value = "settleStatus")Integer settleStatus, Page page){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return buyContractSettlementService.getSettleListBuyContract(settleStatus, userInfo,pagination);
    }

    @GetMapping("/getContractSettleInfo")
    @ApiOperation(value = "根据合同业务id查询合同结算详情", notes = "根据合同业务id查询合同结算详情 ")
    @ApiImplicitParam(name = "contractBusinessId",value = "合同业务id" , required = true)
    public ResponseResult<BuyContractSettleInfoDTO> getContractSettleInfo(String contractBusinessId) {
        return buyContractSettlementService.getContractSettleInfo(contractBusinessId);
    }

    @PostMapping(value="/saveContractSettleInfo")
    @ApiOperation(value="保存合同结算情况",notes="保存合同结算情况")
    public ResponseResult<ResCodeEnum> saveContractSettleInfo(@RequestHeader(value = "sysToken")String sysToken,
                                                        @RequestBody() BuyContractSettlementRQ contractSettlementRQ){
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractSettlementService.saveContractSettleInfo(contractSettlementRQ, userInfo);
    }

    @PostMapping(value="/sureSettleWeight")
    @ApiOperation(value="确认合同重量结算",notes="确认合同重量结算")
    public ResponseResult<ResCodeEnum> sureSettleWeight(@RequestHeader(value = "sysToken")String sysToken,
                                                              @RequestBody() BuyContractSettleSureRq contractSettleSureRq){
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractSettlementService.sureSettleWeight(contractSettleSureRq.getContractSettlementBusinessId(), userInfo);
    }

    @PostMapping(value="/sureContractSettle")
    @ApiOperation(value="合同确认结算",notes="合同确认结算")
    public ResponseResult<ResCodeEnum> sureContractSettle(@RequestHeader(value = "sysToken")String sysToken,
                                                        @RequestBody() BuyContractSettleSureRq contractSettleSureRq){
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractSettlementService.sureContractSettle(contractSettleSureRq.getContractBusinessId(), userInfo);
    }

    @GetMapping("/getSettleListBuy")
    @ApiOperation(value = "采购结算列表 settleStatus：0未结算 1已结算", notes = "采购结算列表 settleStatus：0未结算 1已结算")
    public ResponseResult<List<BuyContractSettleListDTO>> getSettleListBuy(@RequestHeader(value = "sysToken")String sysToken,
                                                                           @Valid BuySettleListRq rq, Page page) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return buyContractSettlementService.getSettleListBuy(userInfo,rq,pagination);
    }

    @GetMapping("/getSettleBuyPopupWindow")
    @ApiOperation(value = "采购结算弹窗 settleStatus：0未结算 1已结算", notes = "采购结算弹窗 settleStatus：0未结算 1已结算")
    public ResponseResult<BuyContractSettlePopupDTO> getSettleBuyPopupWindow(@RequestHeader(value = "sysToken")String sysToken,
                                                                             @RequestParam(value = "contractBusinessId")String contractBusinessId,
                                                                             @RequestParam(value = "settleStatus")Integer settleStatus,
                                                                             Integer settleId) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractSettlementService.getSettleBuyPopupWindow(userInfo,contractBusinessId,settleStatus,settleId);
    }

    @PostMapping(value="/saveSettleBuyPopupWindow")
    @ApiOperation(value="采购结算弹窗结算",notes="采购结算弹窗结算")
    public ResponseResult<ResCodeEnum> saveSettleBuyPopupWindow(@RequestHeader(value = "sysToken")String sysToken,
                                                          @RequestBody() BuySettlePopupWindowSaveRq rq){
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractSettlementService.saveSettleBuyPopupWindow(userInfo,rq);
    }

}

