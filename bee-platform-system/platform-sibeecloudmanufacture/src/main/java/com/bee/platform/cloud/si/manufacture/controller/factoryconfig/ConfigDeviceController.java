package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigDeviceDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigDeviceUpdateRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigSearchTypeListRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigDeviceService;
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
 * 设备档案 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configDevice", tags = "C-设备管理相关接口")
@RequestMapping("/configDevice")
public class ConfigDeviceController {



    @Autowired
    private ConfigDeviceService configDeviceService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchDeviceList")
    @ApiOperation(value = "条件搜索设备列表")
    public ResponseResult<List<ConfigDeviceDTO>> searchDeviceList(@RequestHeader("sysToken") String sysToken, String deviceName,Integer status, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return configDeviceService.searchDeviceList(userInfo,deviceName,status,page);
    }


    @GetMapping("/getDeviceListByName")
    @ApiOperation(value = "根据设备名称模糊搜索设备下拉列表--设备巡检使用")
    public ResponseResult<List<ConfigDeviceDTO>> getDeviceListByName(@RequestHeader("sysToken") String sysToken, String deviceName){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        List<ConfigDeviceDTO> dto = configDeviceService.getDeviceListByName(userInfo,deviceName);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "保存设备信息")
    @PostMapping("/saveDevice")
    public ResponseResult<Integer> saveDevice(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigDeviceSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configDeviceService.saveDevice(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改设备信息")
    @PostMapping("/updateDevice")
    public ResponseResult<Integer> updateDevice(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigDeviceUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configDeviceService.updateDevice(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除设备信息")
    @DeleteMapping("/deleteDeviceById")
    public ResponseResult<Integer> deleteDeviceById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configDeviceService.deleteDeviceById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }




    @PostMapping("/getDeviceListByType")
    @ApiOperation(value = "根据类别查询设备列表(设备类型（0 矿热炉 1 环保设备 2 特种设备 3 其他设备） 空为查全部)")
    public ResponseResult<List<ConfigDeviceDTO>> getDeviceListByType(@RequestHeader("sysToken") String sysToken,@RequestBody ConfigSearchTypeListRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ConfigDeviceDTO> dto = configDeviceService.getDeviceListByType(userInfo,rq.getTypes());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }



    @GetMapping("/getDeviceById")
    @ApiOperation(value = "根据id查询设备详情")
    public ResponseResult<ConfigDeviceDTO> getDeviceById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigDeviceDTO dto = configDeviceService.getDeviceById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


}

