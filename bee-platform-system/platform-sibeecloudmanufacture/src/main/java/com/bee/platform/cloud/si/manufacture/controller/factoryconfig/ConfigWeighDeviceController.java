package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigWeighDeviceDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigSearchTypeListRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigWeighDeviceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigWeighDeviceUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigWeighDeviceService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 称重设备档案 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configWeighDevice", tags = "C-称重设备管理相关接口")
@RequestMapping("/configWeighDevice")
public class ConfigWeighDeviceController {



    @Autowired
    private ConfigWeighDeviceService configWeighDeviceService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchWeighDeviceList")
    @ApiOperation(value = "条件查询称重设备列表")
    public ResponseResult<List<ConfigWeighDeviceDTO>> searchWeighDeviceList(@RequestHeader("sysToken") String sysToken, String weighDeviceName, Integer status, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return configWeighDeviceService.searchWeighDeviceList(userInfo,weighDeviceName,status,page);
    }


    @ApiOperation(value = "保存称重设备信息")
    @PostMapping("/saveWeighDevice")
    public ResponseResult<Integer> saveWeighDevice(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigWeighDeviceSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configWeighDeviceService.saveWeighDevice(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改称重设备信息")
    @PostMapping("/updateWeighDevice")
    public ResponseResult<Integer> updateWeighDevice(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigWeighDeviceUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configWeighDeviceService.updateWeighDevice(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除称重设备信息")
    @DeleteMapping("/deleteWeighDeviceById")
    public ResponseResult<Integer> deleteWeighDeviceById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configWeighDeviceService.deleteWeighDeviceById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @GetMapping("/getWeightSelectList")
    @ApiImplicitParams(
            @ApiImplicitParam(name = "type" ,value = "称重设备类型(0 地磅 1行车称)" , required = true)
    )
    @ApiOperation(value = "获取称重设备信息")
    public ResponseResult<List<ConfigWeighDeviceDTO>> getWeightSelectList(@RequestHeader("sysToken") String sysToken,Integer type){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigWeighDeviceDTO> configWeighDeviceDTOList =  configWeighDeviceService.getWeightSelectList(userInfo,type);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, configWeighDeviceDTOList);
    }


    @GetMapping("/getWeighDeviceById")
    @ApiOperation(value = "根据id查询仓库详情")
    public ResponseResult<ConfigWeighDeviceDTO> getWeighDeviceById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigWeighDeviceDTO dto = configWeighDeviceService.getWeighDeviceById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }



    @PostMapping("/getWeighDeviceListByType")
    @ApiOperation(value = "根据类型--查询称重设备列表(称重设备类型(0 地磅 1行车称) 空为查全部)")
    public ResponseResult<List<ConfigWeighDeviceDTO>> getWeighDeviceListByType(@RequestHeader("sysToken") String sysToken,@RequestBody ConfigSearchTypeListRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ConfigWeighDeviceDTO> dto = configWeighDeviceService.getWeighDeviceListByType(userInfo,rq.getTypes());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


}

