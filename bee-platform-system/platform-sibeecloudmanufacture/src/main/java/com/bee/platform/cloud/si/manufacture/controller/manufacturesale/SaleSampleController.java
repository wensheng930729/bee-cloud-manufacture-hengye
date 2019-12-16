package com.bee.platform.cloud.si.manufacture.controller.manufacturesale;


import com.bee.platform.cloud.si.manufacture.rq.FinishSampleSaleRQ;
import com.bee.platform.cloud.si.manufacture.rq.SaveSampleSaleRQ;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleSampleService;
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
 * 销售取样表 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-09-27
 */
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/saleSample")
@Api(value = "buySample", tags = "样品销售相关接口")
public class SaleSampleController {

    @Autowired
    private UserInfoUtils userInfoUtils;
    @Autowired
    private SaleSampleService saleSampleService;

    @PostMapping(value = "/saveSample")
    @ApiOperation(value = "取样-保存取样", notes = "取样-保存取样")
    public ResponseResult<ResCodeEnum> saveSample(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid SaveSampleSaleRQ rq){
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleSampleService.saveSample(rq, userInfo);
    }

    @PostMapping(value = "/finishSample")
    @ApiOperation(value = "取样-完成取样", notes = "取样-完成取样")
    public ResponseResult<ResCodeEnum> finishSample(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid FinishSampleSaleRQ rq){
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleSampleService.finishSample(rq, userInfo);
    }

}

