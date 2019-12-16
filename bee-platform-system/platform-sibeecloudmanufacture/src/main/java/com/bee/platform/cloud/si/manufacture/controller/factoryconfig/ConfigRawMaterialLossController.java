package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigRawMaterialLossDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRawMaterialLossSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRawMaterialLossUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRawMaterialLossService;
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
 * 原料损耗配置表 前端控制器
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */

@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "configRawMaterialLoss", tags = "C-原料损耗配置相关接口")
@RestController
@RequestMapping("/configRawMaterialLoss")
public class ConfigRawMaterialLossController {



    @Autowired
    private ConfigRawMaterialLossService rawMaterialLossService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchRawMaterialLossList")
    @ApiOperation(value = "条件查询原料损耗列表")
    public ResponseResult<List<ConfigRawMaterialLossDTO>> searchRawMaterialLossList(@RequestHeader("sysToken") String sysToken, String productName, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return rawMaterialLossService.searchRawMaterialLossList(userInfo,productName,page);
    }


    @ApiOperation(value = "保存原料损耗信息")
    @PostMapping("/saveRawMaterialLoss")
    public ResponseResult<Integer> saveRawMaterialLoss(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigRawMaterialLossSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = rawMaterialLossService.saveRawMaterialLoss(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改原料损耗信息")
    @PostMapping("/updateRawMaterialLoss")
    public ResponseResult<Integer> updateRawMaterialLoss(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigRawMaterialLossUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = rawMaterialLossService.updateRawMaterialLoss(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除原料损耗信息")
    @DeleteMapping("/deleteRawMaterialLossById/{id}")
    public ResponseResult<Integer> deleteRawMaterialLossById(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        rawMaterialLossService.deleteRawMaterialLossById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }



    @GetMapping("/getRawMaterialLossById/{id}")
    @ApiOperation(value = "根据id查询原料损耗详情")
    public ResponseResult<ConfigRawMaterialLossDTO> getRawMaterialLossById(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigRawMaterialLossDTO dto = rawMaterialLossService.getRawMaterialLossById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }
}

