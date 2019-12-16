package com.bee.platform.cloud.si.manufacture.controller.manufactureproduce;

import com.bee.platform.cloud.si.manufacture.dto.ActualPfDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProDeviceInspectionStatisticDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceDataDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceRateDTO;
import com.bee.platform.cloud.si.manufacture.rq.ProStatisticRQ;
import com.bee.platform.cloud.si.manufacture.service.BigScreenStatistic.BigScreenStatisticService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProStatisticService;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author xin.huang
 * @description
 * @date 2019/10/18
 */
@RestController
@RequestMapping("/proStatisticController")
@CrossOrigin(origins = "*")
@Api(value = "ProStatisticController",tags = "生产数据统计相关接口")
public class ProStatisticController {

    @Autowired
    private ProStatisticService statisticService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private BigScreenStatisticService bigScreenStatisticService;

    @PostMapping("/recoveryRate")
    @ApiOperation(value = "条件查询生产数据回收率")
    public ResponseResult<List<ProFurnaceRateDTO>> findFurnaceRecoveryRate(@RequestHeader("sysToken") String sysToken,
                                                                           @RequestBody ProStatisticRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return statisticService.findFurnaceRecoveryRate(rq, userInfo);
    }

    @PostMapping("/production")
    @ApiOperation(value = "条件查询矿热炉产量")
    public ResponseResult<ProFurnaceDataDTO> findFurnaceProduction(@RequestHeader("sysToken") String sysToken,
                                                                             @RequestBody ProStatisticRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return statisticService.findFurnaceProduction(rq, userInfo);
    }

    @PostMapping("/productSpec")
    @ApiOperation(value = "条件查询矿热炉产出质量")
    public ResponseResult<ProFurnaceDataDTO> findFurnaceProductSpec(@RequestHeader("sysToken") String sysToken,
                                                                                      @RequestBody ProStatisticRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return statisticService.findProductSpecs(rq, userInfo);
    }

    @PostMapping("/materialConsume")
    @ApiOperation(value = "条件查询矿热炉原料消耗")
    public ResponseResult<ProFurnaceDataDTO> findMaterialConsume(@RequestHeader("sysToken") String sysToken,
                                                                                      @RequestBody ProStatisticRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return statisticService.findMaterialConsume(rq, userInfo);
    }

    @PostMapping("/materialTonConsume")
    @ApiOperation(value = "条件查询矿热炉主料（辅料）吨耗")
    public ResponseResult<ProFurnaceDataDTO> findMaterialTonConsume(@RequestHeader("sysToken") String sysToken,
                                                                                      @RequestBody ProStatisticRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return statisticService.findMaterialTonConsume(rq, userInfo);
    }

    @PostMapping("/powerConsume")
    @ApiOperation(value = "条件查询矿热炉电力消耗")
    public ResponseResult<ProFurnaceDataDTO> findPowerConsume(@RequestHeader("sysToken") String sysToken,
                                                                            @RequestBody ProStatisticRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return statisticService.findPowerConsume(rq, userInfo);
    }

    @PostMapping("/powerTonConsume")
    @ApiOperation(value = "条件查询矿热炉吨电耗")
    public ResponseResult<ProFurnaceDataDTO> findPowerTonConsume(@RequestHeader("sysToken") String sysToken,
                                                                            @RequestBody ProStatisticRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return statisticService.findPowerTonConsume(rq, userInfo);
    }

    @GetMapping("/deviceInspection")
    @ApiOperation(value = "统计设备检修状况")
    public ResponseResult<ProDeviceInspectionStatisticDTO> findStatisticDeviceInspection(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return statisticService.findStatisticDeviceInspection(userInfo);
    }

    @NotIntercept
    @GetMapping("/powerFactor")
    @ApiOperation(value = "查询矿热炉实时功率因素")
    public ResponseResult<List<ActualPfDTO>> findPowerFactor() {
        AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo().setFactoryId(78);
        long endTime = System.currentTimeMillis() / 1000;
        List<ActualPfDTO> results = bigScreenStatisticService.findActualPfc(userInfo.getFactoryId().toString(), endTime);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, results);
    }
}
