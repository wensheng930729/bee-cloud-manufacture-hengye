package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigLocationDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigLocationSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigLocationUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigLocationService;
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
 * 物流地点管理表 前端控制器
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */

@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "configLocation", tags = "C-物流地点管理相关接口")
@RestController
@RequestMapping("/configLocation")
public class ConfigLocationController {


    @Autowired
    private ConfigLocationService locationService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchLocationList")
    @ApiOperation(value = "条件查询物流地点列表")
    public ResponseResult<List<ConfigLocationDTO>> searchLocationList(@RequestHeader("sysToken") String sysToken, String name, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return locationService.searchLocationList(userInfo, name, page);
    }

    @GetMapping("/getLocationList")
    @ApiOperation(value = "物流地点列表")
    public ResponseResult<List<ConfigLocationDTO>> getLocationList(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        List<ConfigLocationDTO> dto = locationService.getLocationList(userInfo);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "保存物流地点信息")
    @PostMapping("/saveLocation")
    public ResponseResult<Integer> saveLocation(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigLocationSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)|| ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = locationService.saveLocation(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改物流地点信息")
    @PostMapping("/updateLocation")
    public ResponseResult<Integer> updateLocation(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigLocationUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)|| ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = locationService.updateLocation(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除物流地点信息")
    @DeleteMapping("/deleteLocationById/{id}")
    public ResponseResult<Integer> deleteLocationById(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)|| ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        locationService.deleteLocationById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @GetMapping("/getLocationById/{id}")
    @ApiOperation(value = "根据id查询物流地点详情")
    public ResponseResult<ConfigLocationDTO> getLocationById(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)|| ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigLocationDTO dto = locationService.getLocationById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }
}

