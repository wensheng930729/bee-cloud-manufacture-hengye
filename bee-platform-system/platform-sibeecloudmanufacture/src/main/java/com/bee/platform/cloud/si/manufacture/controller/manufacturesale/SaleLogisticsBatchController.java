package com.bee.platform.cloud.si.manufacture.controller.manufacturesale;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.SaleContractListContentDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleLogisticsBatchDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleLogisticsInfoDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleNewLogisticsDTO;
import com.bee.platform.cloud.si.manufacture.rq.NewLogisticsRq;
import com.bee.platform.cloud.si.manufacture.rq.SaveLogisticsRq;
import com.bee.platform.cloud.si.manufacture.rq.SureLogisticsRq;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleLogisticsBatchService;
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
 * 物流批次表(销售) 前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "saleLogisticsBatch", tags = "销售-物流批次相关接口")
@RequestMapping("/saleLogisticsBatch")
public class SaleLogisticsBatchController {

    @Autowired
    private SaleLogisticsBatchService logisticsBatchService;

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
    public ResponseResult<SaleLogisticsInfoDTO> getLogisticsBatchList(String contractBusinessId) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                logisticsBatchService.getLogisticsBatchList(contractBusinessId));
    }

    @PostMapping("/getLogisticsContractInfo")
    @ApiOperation(value = "新增合同物流批次信息", notes = "新增合同物流批次信息 ")
    public ResponseResult<SaleLogisticsBatchDTO> getLogisticsContractInfo(HttpServletRequest request,
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
    public ResponseResult<SaleNewLogisticsDTO> getLogisticsContractDetail(HttpServletRequest request, NewLogisticsRq rq) {
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
            @ApiImplicitParam(name = "purchaserMode",value = "采购方式 0自提 1包运" , required = true)
    })
    public ResponseResult<List<SaleContractListContentDTO>> getLogisticsContractList(HttpServletRequest request, Page page,
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

    @GetMapping("/getLogisticsBatchSectionInfo")
    @ApiOperation(value = "查询物流批次阶段信息", notes = "查询物流批次阶段信息 ")
    @ApiImplicitParam(name = "contractBusinessId",value = "合同业务id" , required = true)
    public ResponseResult<SaleLogisticsInfoDTO> getLogisticsBatchSectionInfo(String contractBusinessId) {
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

