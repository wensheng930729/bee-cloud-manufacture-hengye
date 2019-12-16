package com.bee.platform.cloud.si.manufacture.controller;

import com.bee.platform.cloud.si.manufacture.dto.TonBagDetailDTO;
import com.bee.platform.cloud.si.manufacture.rq.BaggingUpdateRq;
import com.bee.platform.cloud.si.manufacture.service.MixBagService;
import com.bee.platform.cloud.user.dto.MixBagDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;

/**
 * @ClassName: MixBagController
 * @Description: 混袋相关接口
 * @Author: fei.sun
 * @Date: 2019/11/25 15:17
 * @Version: 1.0
 */
@RestController
@RequestMapping(value = "/mixBag")
@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "混袋相关接口", tags = "混袋相关接口")
public class MixBagController {

    private final MixBagService mixBagService;

    private final UserInfoUtils userInfoUtils;

    @Autowired
    private MixBagController(UserInfoUtils userInfoUtils,MixBagService mixBagService){
        this.userInfoUtils = userInfoUtils;
        this.mixBagService = mixBagService;
    }

    @PostMapping("/confirmMixBag")
    @ApiOperation("确定混袋")
    public ResponseResult<String> confirmMixBag(@RequestHeader("sysToken") String sysToken, @RequestBody MixBagDTO mixBagDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        String majorTonBagNum = mixBagDTO.getMajorTonBagNum();
        String tonBagNum = mixBagDTO.getTonBagNum();
        BigDecimal mixBagAmount = mixBagDTO.getMixBagAmount();
        mixBagService.confirmMixBag(majorTonBagNum,tonBagNum,mixBagAmount,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @GetMapping("/getTonBagDetail")
    @ApiOperation("查询吨袋相关详细信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "tonBagNum",value = "吨袋编号",required = true)
    })
    public ResponseResult<TonBagDetailDTO> getTonBagDetail(@RequestHeader("sysToken") String sysToken, String tonBagNum){
        userInfoUtils.getUserInfo(sysToken);
        return mixBagService.getTonBagDetail(tonBagNum);
    }

    @PostMapping("/updateTonBagInfo")
    @ApiOperation("更新吨袋相关信息")
    public ResponseResult<String> updateTonBagInfo(@RequestHeader("sysToken") String sysToken, @RequestBody BaggingUpdateRq rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        return mixBagService.updateTonBagInfo(rq, userInfo);
    }

}
