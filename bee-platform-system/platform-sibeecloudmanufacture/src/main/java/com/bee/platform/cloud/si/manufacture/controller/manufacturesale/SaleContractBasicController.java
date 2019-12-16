package com.bee.platform.cloud.si.manufacture.controller.manufacturesale;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.SaleContractDetailTotalDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleContractListDTO;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleContractBasicService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * <p>
 * 销售合同信息表 前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-26
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "saleContractBasic", tags = "销售-合同相关接口")
@RequestMapping("/saleContractBasic")
public class SaleContractBasicController {
    @Autowired
    private SaleContractBasicService saleContractBasicService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping(value="/addContractBuy")
    @ApiOperation(value="销售合同新增",notes="销售合同新增")
    public ResponseResult<ResCodeEnum> addContractBuy(@RequestHeader(value = "sysToken")String sysToken, @RequestBody() @Valid SaleContractAddRq rq){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if(ObjectUtils.isEmpty(rq)){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return saleContractBasicService.addContractBuy(rq, userInfo);
    }

    @GetMapping(value = "/getBuyContractList")
    @ApiOperation(value = "销售合同列表", notes = "销售合同列表")
    public ResponseResult<SaleContractListDTO> getBuyContractList(@RequestHeader(value = "sysToken")String sysToken,
                                                                  @Valid BuyContractListRq rq, Page page) {
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return saleContractBasicService.getSaleContractList(rq,pagination,userInfo);
    }

    @GetMapping(value = "/getSaleContractDetail")
    @ApiOperation(value = "销售合同详情", notes = "销售合同详情")
    public ResponseResult<SaleContractDetailTotalDTO> getSaleContractDetail(@RequestHeader(value = "sysToken")String sysToken,
                                                                            @RequestParam(value = "contractBusinessId") String contractBusinessId) {
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleContractBasicService.getSaleContractDetail(contractBusinessId,userInfo);
    }

    @PostMapping(value="/receiveForSaleContract")
    @ApiOperation(value="销售合同收款",notes="销售合同收款")
    public ResponseResult<ResCodeEnum> receiveForSaleContract(@RequestHeader(value = "sysToken")String sysToken
                                                        , @RequestBody() @Valid SaleContractReceiveRq rq){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if(ObjectUtils.isEmpty(rq)){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return saleContractBasicService.receiveForSaleContract(rq, userInfo);
    }

    @PostMapping(value="/updateSettleAmountSaleContract")
    @ApiOperation(value="销售合同修改结算单价和金额",notes="销售合同修改结算单价和金额")
    public ResponseResult<ResCodeEnum> updateSettleAmountSaleContract(@RequestHeader(value = "sysToken")String sysToken,
                                                             @RequestBody() @Valid BuyContractSettleUpdateAmountRq rq){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if(ObjectUtils.isEmpty(rq)){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return saleContractBasicService.updateSettleAmountSaleContract(rq, userInfo);
    }

    @PostMapping(value="/confirmSettleSaleContract")
    @ApiOperation(value="销售合同结算确认",notes="销售合同结算确认")
    public ResponseResult<ResCodeEnum> confirmSettleSaleContract(@RequestHeader(value = "sysToken")String sysToken,
                                                                @RequestParam(value = "contractSettlementBusinessId")String businessId){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleContractBasicService.confirmSettleSaleContract(businessId, userInfo);
    }

    @PostMapping(value="/completeSaleContract")
    @ApiOperation(value="销售合同完成",notes="销售合同完成")
    public ResponseResult<ResCodeEnum> completeSaleContract(@RequestHeader(value = "sysToken")String sysToken,
                                                            @RequestBody() @Valid SaleContractCompleteRq rq){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleContractBasicService.completeSaleContract(rq, userInfo);
    }
}

