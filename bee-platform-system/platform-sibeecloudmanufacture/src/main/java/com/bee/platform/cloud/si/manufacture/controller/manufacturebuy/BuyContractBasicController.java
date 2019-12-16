package com.bee.platform.cloud.si.manufacture.controller.manufacturebuy;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractDetailTotalDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractListDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProductDTO;
import com.bee.platform.cloud.si.manufacture.dto.SupplierOrCustomersDTO;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyContractBasicService;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 采购合同信息表 前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "buyContractBasic", tags = "采购-合同相关接口")
@RequestMapping("/buyContractBasic")
public class BuyContractBasicController {

    @Autowired
    private BuyContractBasicService buyContractBasicService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping(value="/addContractBuy")
    @ApiOperation(value="采购合同新增",notes="采购合同新增")
    public ResponseResult<ResCodeEnum> addContractBuy(@RequestHeader(value = "sysToken")String sysToken, @RequestBody() @Valid BuyContractAddRq rq){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if(ObjectUtils.isEmpty(rq)){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return buyContractBasicService.addContractBuy(rq, userInfo);
    }

    @GetMapping(value = "/getBuyContractList")
    @ApiOperation(value = "采购合同列表", notes = "采购合同列表")
    public ResponseResult<BuyContractListDTO> getBuyContractList(@RequestHeader(value = "sysToken")String sysToken,
                                                                 @Valid BuyContractListRq rq, Page page) {
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return buyContractBasicService.getBuyContractList(rq,pagination,userInfo);
    }

    @GetMapping(value = "/getBuyContractDetail")
    @ApiOperation(value = "采购合同详情", notes = "采购合同详情")
    public ResponseResult<BuyContractDetailTotalDTO> getBuyContractDetail(@RequestHeader(value = "sysToken")String sysToken,
                                                                          @RequestParam(value = "contractBusinessId") String contractBusinessId) {
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractBasicService.getBuyContractDetail(contractBusinessId,userInfo);
    }

    @PostMapping(value="/payForBuyContractBasic")
    @ApiOperation(value="采购合同付款",notes="采购合同付款")
    public ResponseResult<ResCodeEnum> payForBuyContract(@RequestHeader(value = "sysToken")String sysToken
                                                            , @RequestBody() @Valid BuyContractPayRq rq){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if(ObjectUtils.isEmpty(rq)){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return buyContractBasicService.payForBuyContract(rq, userInfo);
    }

    @PostMapping(value="/updateSettleAmountBuyContract")
    @ApiOperation(value="采购合同修改结算单价和金额",notes="采购合同修改结算单价和金额")
    public ResponseResult<ResCodeEnum> updateSettleAmountBuy(@RequestHeader(value = "sysToken")String sysToken,
                                                             @RequestBody() @Valid BuyContractSettleUpdateAmountRq rq){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if(ObjectUtils.isEmpty(rq)){
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return buyContractBasicService.updateSettleAmountBuy(rq, userInfo);
    }

    @PostMapping(value="/confirmSettleBuyContract")
    @ApiOperation(value="采购合同结算确认",notes="采购合同结算确认")
    public ResponseResult<ResCodeEnum> confirmSettleBuyContract(@RequestHeader(value = "sysToken")String sysToken,
                                @RequestParam(value = "contractSettlementBusinessId")String businessId){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractBasicService.confirmSettleBuyContract(businessId, userInfo);
    }

    @PostMapping(value="/completeBuyContract")
    @ApiOperation(value="采购合同完成",notes="采购合同完成")
    public ResponseResult<ResCodeEnum> completeBuyContract(@RequestHeader(value = "sysToken")String sysToken,
                                                           @RequestBody() @Valid SaleContractCompleteRq rq){
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractBasicService.completeBuyContract(rq.getBusinessId(), userInfo);
    }

    @GetMapping(value = "/getProducts")
    @ApiOperation(value = "获取所有产品", notes = "获取所有产品")
    public ResponseResult<List<ProductDTO>> getProducts(@RequestHeader(value = "sysToken")String sysToken) {
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractBasicService.getProducts(userInfo);
    }

    @GetMapping(value = "/getSuppliersOrCustomers")
    @ApiOperation(value = "获取所有供应商或客户 1采购 2销售", notes = "获取所有供应商或客户 1采购 2销售")
    public ResponseResult<List<SupplierOrCustomersDTO>> getSuppliersOrCustomers(@RequestHeader(value = "sysToken")String sysToken,
                                                                                @RequestParam(value = "type")Integer type) {
        // 获取用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyContractBasicService.getSuppliersOrCustomers(userInfo,type);
    }

}

