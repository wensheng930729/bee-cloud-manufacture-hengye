package com.bee.platform.cloud.si.manufacture.controller.manufacturebuy;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.BuyCarrierInfoDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyCarrierTransportDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyTransportReportDTO;
import com.bee.platform.cloud.si.manufacture.rq.BuyCarrierTransportSearchRQ;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyCarrierTransportService;
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

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 承运方运输表(采购) 前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "buyCarrierTransport", tags = "采购-运输段承运方相关接口")
@RequestMapping("/buyCarrierTransport")
public class BuyCarrierTransportController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private BuyCarrierTransportService carrierTransportService;

    @PostMapping(value="/saveCarrierTransport")
    @ApiOperation(value="保存运输段承运方信息",notes="保存运输段承运方信息")
    public ResponseResult<ResCodeEnum> saveCarrierTransport(HttpServletRequest request,
                                                            @RequestBody() BuyCarrierTransportDTO carrierTransportDTO){
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return carrierTransportService.saveCarrierTransport(carrierTransportDTO, userInfo);
    }

    @GetMapping(value = "/getCarrierTransportByCarrier")
    @ApiOperation(value = "根据承运方查询运输段承运方信息列表", notes = "根据承运方查询运输段承运方信息列表")
    public ResponseResult<List<BuyCarrierTransportDTO>> getCarrierTransportByCarrier(HttpServletRequest request, Page page) {
        // 获取用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return carrierTransportService.getCarrierTransportByCarrier(pagination, userInfo);
    }

    @GetMapping(value = "/getCarrierInfoByTransportId")
    @ApiOperation(value = "根据承运方id查询承运方相关运输段信息", notes = "根据承运方id查询承运方相关运输段信息")
    @ApiImplicitParam(name = "carrierTransportId",value = "承运方id" , required = true)
    public ResponseResult<BuyCarrierInfoDTO> getCarrierInfoByTransportId(String carrierTransportId) {
        return carrierTransportService.getCarrierInfoByTransportId(carrierTransportId);
    }

    @PostMapping(value="/saveCarrierTransportSection")
    @ApiOperation(value="承运方保存运输段承运方信息",notes="承运方保存运输段承运方信息")
    public ResponseResult<ResCodeEnum> saveCarrierTransportSection(HttpServletRequest request,
                                                            @RequestBody() BuyCarrierTransportDTO carrierTransportDTO){
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return carrierTransportService.saveCarrierTransportSection(carrierTransportDTO, userInfo);
    }

}

