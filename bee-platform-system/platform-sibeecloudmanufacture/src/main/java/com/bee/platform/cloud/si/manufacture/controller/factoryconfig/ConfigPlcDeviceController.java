package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigPlcDeviceDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigPlcDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigPlcDeviceUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigPlcDeviceService;
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
 * PLC设备档案 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configPlcDevice", tags = "C-PLC设备管理相关接口")
@RequestMapping("/configPlcDevice")
public class ConfigPlcDeviceController {



    @Autowired
    private ConfigPlcDeviceService configPlcDeviceService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchPlcDeviceList")
    @ApiOperation(value = "条件查询PLC设备列表")
    public ResponseResult<List<ConfigPlcDeviceDTO>> searchPlcDeviceList(@RequestHeader("sysToken") String sysToken, String deviceName, Integer status, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return configPlcDeviceService.searchPlcDeviceList(userInfo,deviceName,status,page);
    }


    @ApiOperation(value = "保存PLC设备信息")
    @PostMapping("/savePlcDevice")
    public ResponseResult<ResCodeEnum> savePlcDevice(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigPlcDeviceSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return  configPlcDeviceService.savePlcDevice(userInfo, rq);

    }


    @ApiOperation(value = "修改PLC设备信息")
    @PostMapping("/updatePlcDevice")
    public ResponseResult<ResCodeEnum> updatePlcDevice(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigPlcDeviceUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        return configPlcDeviceService.updatePlcDevice(userInfo, rq);

    }


    @ApiOperation(value = "根据id删除PLC设备信息")
    @DeleteMapping("/deletePlcDeviceById")
    public ResponseResult<Integer> deletePlcDeviceById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configPlcDeviceService.deletePlcDeviceById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }



    @GetMapping("/getPlcDeviceList")
    @ApiOperation(value = "查询企业下启用的PLC设备列表")
    public ResponseResult<List<ConfigPlcDeviceDTO>> getPlcDeviceList(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigPlcDeviceDTO> dto= configPlcDeviceService.getPlcDeviceList(userInfo);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @GetMapping("/getPlcDeviceById")
    @ApiOperation(value = "根据id查询PLC设备详情")
    public ResponseResult<ConfigPlcDeviceDTO> getPlcDeviceById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigPlcDeviceDTO dto = configPlcDeviceService.getPlcDeviceById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }



}

