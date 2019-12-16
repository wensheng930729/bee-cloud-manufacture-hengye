package com.bee.platform.cloud.si.manufacture.controller.manufactureproduce;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.ProBaggingDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProBaggingDeatilsDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProBaggingFirstDTO;
import com.bee.platform.cloud.si.manufacture.rq.ProBaggingFirstRq;
import com.bee.platform.cloud.si.manufacture.rq.ProBaggingSecondRq;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProBaggingService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;


/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 成品装袋
 * @Date 2019/10/9 10:03
 */
@RestController
@RequestMapping("/proBagging")
@CrossOrigin(origins = "*")
@Api(value = "proBagging", tags = "成品装袋")
public class ProBaggingController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    @Autowired
    private ProBaggingService proBaggingService;

    @PostMapping(value = "/saveBaggingStepOne")
    @ApiOperation(value = "保存成品装袋（第一步）", notes = "保存成品装袋（第一步）")
    public ResponseResult<ProBaggingFirstDTO> saveBaggingStepOne(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ProBaggingFirstRq rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return proBaggingService.saveBaggingStepOne(rq, userInfo);
    }


    @PostMapping(value = "/saveBaggingStepTwo")
    @ApiOperation(value = "保存成品装袋（第二步）", notes = "保存成品装袋（第二步）")
    public ResponseResult<String> saveBaggingStepTwo(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ProBaggingSecondRq rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return proBaggingService.saveBaggingStepTwo(rq, userInfo);
    }


    @GetMapping(value = "/getBaggingResults")
    @ApiOperation(value = "获取装袋记录列表", notes = "获取装袋记录列表")
    public ResponseResult<List<ProBaggingDTO>> getBaggingResults(@RequestHeader("sysToken") String sysToken, Page page) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        List<ProBaggingDTO> proBaggingDTOS = proBaggingService.getBaggingResults(userInfo, pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, proBaggingDTOS, PageUtils.transToPage(pagination));
    }


    @PostMapping(value = "/getBaggingDeatils")
    @ApiOperation(value = "获取装袋记录详细信息", notes = "获取装袋记录详细信息")
    public ResponseResult<ProBaggingDeatilsDTO> getBaggingDeatils(@RequestHeader("sysToken") String sysToken, @RequestBody ProBaggingDTO rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ProBaggingDeatilsDTO proBaggingDeatilsDTO = proBaggingService.getBaggingDeatils(rq,userInfo);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, proBaggingDeatilsDTO);
    }
}

