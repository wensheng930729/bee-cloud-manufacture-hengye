package com.bee.platform.cloud.si.manufacture.controller;


import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.SampleService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;


/**
 * <p>
 * 采购样品相关接口
 * </p>
 *
 * @author liliang123
 * @since 2019-09-26
 */
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/sample")
@Api(value = "sample", tags = "样品全部业务线公用接口")
public class SampleController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    @Autowired
    private SampleService sampleService;

    @GetMapping(value = "/getSamplePrepareList")
    @ApiOperation(value = "样品待取样列表", notes = "样品待取样列表")
    public ResponseResult<List<SamplePrepareDTO>> getSamplePrepareList(@RequestHeader("sysToken") String sysToken, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return sampleService.getSamplePrepareList(userInfo, page);
    }

    @GetMapping(value = "/getSampleAlreadyList")
    @ApiOperation(value = "样品已取样列表", notes = "样品已取样列表")
    public ResponseResult<List<SampleAlreadyDTO>> getSampleAlreadyList(@RequestHeader("sysToken") String sysToken, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return sampleService.getSampleAlreadyList(userInfo, page);
    }

    @GetMapping(value = "/getAssayList")
    @ApiImplicitParam(value = "assayStatus", name = "样品化验状态0已弃用1待化验2化验中3已化验")
    @ApiOperation(value = "样品化验列表", notes = "样品化验列表")
    public ResponseResult getAssayList(@RequestHeader("sysToken") String sysToken, Integer assayStatus, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (assayStatus == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return sampleService.getAssayList(userInfo, assayStatus, page);
    }

    @PostMapping(value = "/abandonSample")
    @ApiOperation(value = "化验人员弃用样品", notes = "化验人员弃用样品")
    public ResponseResult<ResCodeEnum> abandonSample(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid SampleAssayAbandonRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return sampleService.abandonSample(userInfo, rq);
    }

    @PostMapping(value = "/startAssaySample")
    @ApiOperation(value = "化验人员开始化验样品", notes = "化验人员开始化验样品")
    public ResponseResult<ResCodeEnum> startAssaySample(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid SampleAssayStartRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return sampleService.startAssaySample(userInfo, rq);
    }

    @PostMapping(value = "/saveAssayResult")
    @ApiOperation(value = "保存样品化验结果", notes = "保存样品化验结果")
    public ResponseResult<ResCodeEnum> saveAssayResult(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid SampleAssayResultSaveRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return sampleService.saveAssayResult(userInfo, rq);
    }

    @PostMapping(value = "/saveTemporaryAssayResult")
    @ApiOperation(value = "化验保存临时化验结果", notes = "化验保存临时化验结果")
    public ResponseResult<ResCodeEnum> saveTemporaryAssayResult(@RequestHeader("sysToken") String sysToken,
                                                                @RequestBody @Valid SampleAssayResultSaveRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return sampleService.saveTemporaryAssayResult(userInfo, rq);
    }

    @GetMapping(value = "/getTemporaryAssayResult")
    @ApiOperation(value = "根据样品code获取临时化验结果", notes = "根据样品code获取临时化验结果")
    public ResponseResult<List<SampleAssayResultDTO>> getTemporaryAssayResult(String sampleCode) {
        return sampleService.getTemporaryAssayResult(sampleCode);
    }

    @GetMapping(value = "/getSampleAssayDetailByCode")
    @ApiOperation(value = "根据样品code获取样品详情", notes = "根据样品code获取样品详情")
    public ResponseResult<SampleAssayDetailDTO> getSampleAssayDetailByCode(@RequestHeader("sysToken") String sysToken, @RequestParam(required = true) String sampleCode) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return sampleService.getSampleAssayDetailByCode(userInfo, sampleCode);
    }

    @PostMapping(value = "/generateSampleCode")
    @ApiOperation(value = "生成样品code", notes = "生成样品code")
    public ResponseResult<List<String>> generateSampleCode(@RequestBody @Valid SampleCodeRQ rq) {
        return sampleService.generateSampleCode(rq);
    }

    @GetMapping(value = "/getSampleAssayResultOut")
    @ApiOperation(value = "根据样品code查询样品化验输出结果", notes = "根据样品code查询样品化验输出结果")
    public ResponseResult<List<SampleAssayResultOutDTO>> getSampleAssayResultOut(@RequestHeader("sysToken") String sysToken
            , String sampleCode
            , Integer productId) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (StringUtils.isBlank(sampleCode)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return sampleService.getSampleAssayResultOut(userInfo, sampleCode);
    }

    @PostMapping(value = "/saveSampleProductSpec")
    @ApiOperation(value = "保存样品化验规格", notes = "保存样品化验规格")
    public ResponseResult<ResCodeEnum> saveSampleProductSpec(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid SampleSaveProductSpecRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return sampleService.saveSampleProductSpec(userInfo, rq);
    }

    @GetMapping(value = "/getDetailByCode")
    @ApiOperation(value = "根据二维码编码查询详情", notes = "根据二维码编码查询详情")
    public ResponseResult getDetailByCode(String sampleCode) {
        if (StringUtils.isBlank(sampleCode)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        return sampleService.getDetailByCode(sampleCode);
    }
}
