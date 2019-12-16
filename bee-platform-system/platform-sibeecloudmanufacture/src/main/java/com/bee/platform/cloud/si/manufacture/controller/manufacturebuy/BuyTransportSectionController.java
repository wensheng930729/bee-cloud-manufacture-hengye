package com.bee.platform.cloud.si.manufacture.controller.manufacturebuy;


import com.bee.platform.cloud.si.manufacture.dto.BuyTransportSectionDTO;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyTransportSectionService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
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
 * 物流批次运输段表(采购) 前端控制器
 * </p>
 *
 * @author qhwang
 * @since 2019-09-24
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "buyTransportSection", tags = "采购-物流批次运输段相关接口")
@RequestMapping("/buyTransportSection")
public class BuyTransportSectionController {

    @Autowired
    private BuyTransportSectionService transportSectionService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @PostMapping(value="/saveTransportSection")
    @ApiOperation(value="保存物流批次运输段信息",notes="保存物流批次运输段信息")
    public ResponseResult<ResCodeEnum> saveTransportSection(HttpServletRequest request,
                                                            @RequestBody() BuyTransportSectionDTO transportSectionDTO){
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return transportSectionService.saveTransportSection(transportSectionDTO, userInfo);
    }

    @PostMapping(value="/saveNewTransportSection")
    @ApiOperation(value="新增物流批次运输段信息",notes="新增物流批次运输段信息")
    public ResponseResult<ResCodeEnum> saveNewTransportSection(HttpServletRequest request,
                                                            @RequestBody() BuyTransportSectionDTO transportSectionDTO){
        // 获取当前用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return transportSectionService.saveNewTransportSection(transportSectionDTO, userInfo);
    }

    @GetMapping(value = "/getTransportSectionDetail")
    @ApiOperation(value = "根据运输段id查询运输段信息(到厂的承运商)", notes = "根据运输段id查询运输段信息(到厂的承运商)")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "transportSectionId", value = "批次运输段id", required = true)
    })
    public ResponseResult<BuyTransportSectionDTO> getTransportSectionDetail(HttpServletRequest request,
                                                                           @RequestParam(value = "transportSectionId") String transportSectionId) {
        // 获取用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return transportSectionService.getTransportSectionDetail(transportSectionId);
    }

    @GetMapping(value = "/getTransportSectionAllDetail")
    @ApiOperation(value = "根据运输段id查询运输段信息(全部承运商)", notes = "根据运输段id查询运输段信息(全部承运商)")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "transportSectionId", value = "批次运输段id", required = true)
    })
    public ResponseResult<BuyTransportSectionDTO> getTransportSectionAllDetail(HttpServletRequest request,
                                                                            @RequestParam(value = "transportSectionId") String transportSectionId) {
        // 获取用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return transportSectionService.getTransportSectionAllDetail(transportSectionId);
    }

    @GetMapping(value = "/getTransportSectionById")
    @ApiOperation(value = "根据运输段id单独查询运输段详细信息", notes = "根据运输段id单独查询运输段详细信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "transportSectionId", value = "批次运输段id", required = true)
    })
    public ResponseResult<BuyTransportSectionDTO> getTransportSectionById(HttpServletRequest request,
                                                                            @RequestParam(value = "transportSectionId") String transportSectionId) {
        // 获取用户信息
        String sysToken = WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return transportSectionService.getTransportSectionById(transportSectionId);
    }

}

