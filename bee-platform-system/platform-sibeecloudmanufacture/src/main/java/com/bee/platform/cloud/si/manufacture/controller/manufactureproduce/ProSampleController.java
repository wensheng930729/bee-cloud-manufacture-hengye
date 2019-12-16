package com.bee.platform.cloud.si.manufacture.controller.manufactureproduce;


import com.bee.platform.cloud.si.manufacture.rq.FinishSampleProRQ;
import com.bee.platform.cloud.si.manufacture.rq.SaveSampleProRQ;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProSampleService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * <p>
 * 生产样品表 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-09-27
 */
@RestController
@RequestMapping("/proSample")
@CrossOrigin(origins = "*")
@Api(value = "proSample", tags = "样品生产相关接口")
public class ProSampleController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    @Autowired
    private ProSampleService proSampleService;

    @PostMapping(value = "/saveSample")
    @ApiOperation(value = "取样-保存取样", notes = "取样-保存取样")
    public ResponseResult<ResCodeEnum> saveSample(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid SaveSampleProRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return proSampleService.saveSample(rq, userInfo);
    }

    @PostMapping(value = "/finishSample")
    @ApiOperation(value = "取样-完成取样", notes = "取样-完成取样")
    public ResponseResult<ResCodeEnum> finishSample(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid FinishSampleProRQ rq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return proSampleService.finishSample(rq, userInfo);
    }

}

