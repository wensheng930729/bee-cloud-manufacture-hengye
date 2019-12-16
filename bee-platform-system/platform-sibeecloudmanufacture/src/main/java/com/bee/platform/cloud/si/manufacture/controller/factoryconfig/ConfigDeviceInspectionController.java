package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigDeviceInspectionDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceInspectionSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceInspectionUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigDeviceInspectionService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 设备巡检 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configDeviceInspection", tags = "C-设备巡检相关接口")
@RequestMapping("/configDeviceInspection")
public class ConfigDeviceInspectionController {


    @Autowired
    private ConfigDeviceInspectionService configDeviceInspectionService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchDeviceInspectionList")
    @ApiOperation(value = "条件查询设备巡检列表")
    public ResponseResult<List<ConfigDeviceInspectionDTO>> searchDeviceInspectionList(@RequestHeader("sysToken") String sysToken, String deviceName, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return configDeviceInspectionService.searchDeviceInspectionList(userInfo,deviceName,page);
    }


    @ApiOperation(value = "保存设备巡检信息")
    @PostMapping("/saveDeviceInspection")
    public ResponseResult<Integer> saveDeviceInspection(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigDeviceInspectionSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configDeviceInspectionService.saveDeviceInspection(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改设备巡检信息")
    @PostMapping("/updateDeviceInspection")
    public ResponseResult<Integer> updateDeviceInspection(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigDeviceInspectionUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configDeviceInspectionService.updateDeviceInspection(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除设备巡检信息")
    @DeleteMapping("/deleteDeviceInspectionById")
    public ResponseResult<Integer> deleteDeviceInspectionById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configDeviceInspectionService.deleteDeviceInspectionById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }



    @GetMapping("/checkDeviceInspection")
    @ApiOperation(value = "根据--设备巡检编号--查询企业下--是否有--该设备巡检--true有--false无")
    public ResponseResult<Boolean> checkDeviceInspection(@RequestHeader("sysToken") String sysToken,@RequestParam(value = "deviceCode") String deviceCode){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Boolean result =  configDeviceInspectionService.checkDeviceInspection(userInfo,deviceCode);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }



    @GetMapping("/getDeviceInspectionByCode")
    @ApiOperation(value = "根据--设备巡检编号--查询--设备巡检信息")
    public ResponseResult<ConfigDeviceInspectionDTO> getDeviceInspectionByCode(@RequestHeader("sysToken") String sysToken,@RequestParam(value = "deviceCode") String deviceCode){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigDeviceInspectionDTO dto =  configDeviceInspectionService.getDeviceInspectionByCode(userInfo,deviceCode);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }



    @GetMapping("/getDeviceInspectionById")
    @ApiOperation(value = "根据id查询设备巡检详情")
    public ResponseResult<ConfigDeviceInspectionDTO> getDeviceInspectionById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigDeviceInspectionDTO dto = configDeviceInspectionService.getDeviceInspectionById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}

