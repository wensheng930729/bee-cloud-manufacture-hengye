package com.bee.platform.cloud.si.manufacture.controller.manufacturebuy;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.rq.LogisticsContractListSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.NewLogisticsRq;
import com.bee.platform.cloud.si.manufacture.rq.SaveLogisticsRq;
import com.bee.platform.cloud.si.manufacture.rq.SureLogisticsRq;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyLogisticsBatchService;
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
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 物流批次表(采购) 前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "buyLogisticsBatch", tags = "采购-物流批次相关接口")
@RequestMapping("/buyLogisticsBatch")
public class BuyLogisticsBatchController {

    @Autowired
    private BuyLogisticsBatchService logisticsBatchService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping(value="/saveLogisticsBatch")
    @ApiOperation(value="保存物流批次信息",notes="保存物流批次信息")
    public ResponseResult<ResCodeEnum> saveLogisticsBatchInfo(HttpServletRequest request,
                                                              @RequestBody() SureLogisticsRq rq){
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return logisticsBatchService.saveLogisticsBatchInfo(rq.getBatchId(), userInfo);
    }

    @GetMapping("/getLogisticsBatchInfo")
    @ApiOperation(value = "根据合同业务id查询物流信息", notes = "根据合同业务id查询物流信息 ")
    @ApiImplicitParam(name = "contractBusinessId",value = "合同业务id" , required = true)
    public ResponseResult<BuyLogisticsInfoDTO> getLogisticsBatchList(String contractBusinessId) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                logisticsBatchService.getLogisticsBatchList(contractBusinessId));
    }

    @PostMapping("/getLogisticsContractInfo")
    @ApiOperation(value = "新增合同物流批次信息", notes = "新增合同物流批次信息 ")
    public ResponseResult<BuyLogisticsBatchDTO> getLogisticsContractInfo(HttpServletRequest request,
                                                                       @RequestBody() NewLogisticsRq rq) {
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return logisticsBatchService.getLogisticsContractInfo(rq.getContractBusinessId(), userInfo);
    }

    @GetMapping("/getLogisticsContractDetail")
    @ApiOperation(value = "编辑合同物流批次信息", notes = "编辑合同物流批次信息 ")
    public ResponseResult<BuyNewLogisticsDTO> getLogisticsContractDetail(HttpServletRequest request, NewLogisticsRq rq) {
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return logisticsBatchService.getLogisticsContractDetail(rq.getContractBusinessId(), userInfo);
    }

    @GetMapping(value = "/getLogisticsContractList")
    @ApiOperation(value = "运输合同列表", notes = "运输合同列表")
    @ApiImplicitParams({
        @ApiImplicitParam(name = "keyword",value = "查询关键字-合同编号" , required = false),
        @ApiImplicitParam(name = "purchaserMode",value = "采购方式 0自提 1供方发货" , required = true)
    })
    public ResponseResult<List<BuyContractListContentDTO>> getLogisticsContractList(HttpServletRequest request, Page page,
                                                                                    @RequestParam(value = "keyword",required = false) String keyword,
                                                                                    @RequestParam(value = "purchaserMode") Integer purchaserMode) {
        // 获取用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return logisticsBatchService.getLogisticsContractList(keyword, purchaserMode, pagination, userInfo);
    }

    @GetMapping(value = "/getContractLogisticInfoList")
    @ApiOperation(value = "物流记录列表", notes = "物流人员-物流记录列表")
    public ResponseResult<List<ContractLogisticInfoDTO>> getContractLogisticInfoList(HttpServletRequest request, Page page) {
        // 获取用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return logisticsBatchService.getContractLogisticInfoList(pagination, userInfo);
    }

    @GetMapping(value = "/getAllLogisticsContractList")
    @ApiOperation(value = "分页查询物流订单列表", notes = "分页查询物流订单列表")
    public ResponseResult<List<LogisticsContractListContentDTO>> getAllLogisticsContractList(HttpServletRequest request, Page page,
                                                                                             LogisticsContractListSearchRQ rq) {
        // 获取用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return logisticsBatchService.getAllLogisticsContractList(rq, pagination, userInfo);
    }

    @GetMapping("/getLogisticsBatchSectionInfo")
    @ApiOperation(value = "查询物流批次阶段信息", notes = "查询物流批次阶段信息 ")
    @ApiImplicitParam(name = "contractBusinessId",value = "合同业务id" , required = true)
    public ResponseResult<BuyLogisticsInfoDTO> getLogisticsBatchSectionInfo(String contractBusinessId) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                logisticsBatchService.getLogisticsBatchSection(contractBusinessId));
    }

    @PostMapping("/saveLogisticsSectionInfo")
    @ApiOperation(value = "新增物流批次阶段信息", notes = "新增物流批次阶段信息 ")
    public ResponseResult<String> saveLogisticsSectionInfo(HttpServletRequest request, @RequestBody() SaveLogisticsRq rq) {
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return logisticsBatchService.saveLogisticsSectionInfo(rq, userInfo);
    }

}

