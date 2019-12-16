package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigMaterialsConsumptionDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigMaterialsConsumptionSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigMaterialsConsumptionUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigMaterialsConsumptionService;
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
 * 原料吨耗 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configMaterialsConsumption", tags = "C-原料吨耗相关接口")
@RequestMapping("/configMaterialsConsumption")
public class ConfigMaterialsConsumptionController {


    @Autowired
    private ConfigMaterialsConsumptionService configMaterialsConsumptionService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchMaterialsConsumptionList")
    @ApiOperation(value = "条件查询原料吨耗列表")
    public ResponseResult<List<ConfigMaterialsConsumptionDTO>> searchMaterialsConsumptionList(@RequestHeader("sysToken") String sysToken, String productName, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return configMaterialsConsumptionService.searchMaterialsConsumptionList(userInfo,productName,page);
    }


    @ApiOperation(value = "保存原料吨耗信息")
    @PostMapping("/saveMaterialsConsumption")
    public ResponseResult<Integer> saveMaterialsConsumption(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigMaterialsConsumptionSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configMaterialsConsumptionService.saveMaterialsConsumption(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改原料吨耗信息")
    @PostMapping("/updateMaterialsConsumption")
    public ResponseResult<Integer> updateMaterialsConsumption(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigMaterialsConsumptionUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configMaterialsConsumptionService.updateMaterialsConsumption(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除原料吨耗信息")
    @DeleteMapping("/deleteMaterialsConsumptionById")
    public ResponseResult<Integer> deleteMaterialsConsumptionById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configMaterialsConsumptionService.deleteMaterialsConsumptionById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }



    @GetMapping("/getMaterialsConsumptionById")
    @ApiOperation(value = "根据id查询原料吨耗详情")
    public ResponseResult<ConfigMaterialsConsumptionDTO> getMaterialsConsumptionById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigMaterialsConsumptionDTO dto = configMaterialsConsumptionService.getMaterialsConsumptionById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }
}

