package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigElectricityPriceDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigElectricityPriceService;
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
 *  电价管理 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configElectricityPrice", tags = "C-电价管理相关接口")
@RequestMapping("/configElectricityPrice")
public class ConfigElectricityPriceController {


    @Autowired
    private ConfigElectricityPriceService  configElectricityPriceService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchElectricityPriceList")
    @ApiOperation(value = "条件查询电表列表")
    public ResponseResult<List<ConfigElectricityPriceDTO>> searchElectricityPriceList(@RequestHeader("sysToken") String sysToken, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return configElectricityPriceService.searchElectricityPriceList(userInfo,page);
    }


    @ApiOperation(value = "保存电价信息")
    @PostMapping("/saveElectricityPrice")
    public ResponseResult<Integer> saveElectricityPrice(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigElectricityPriceSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configElectricityPriceService.saveElectricityPrice(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }



    @ApiOperation(value = "修改电价信息")
    @PostMapping("/updateElectricityPrice")
    public ResponseResult<Integer> updateElectricityPrice(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigElectricityPriceUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configElectricityPriceService.updateElectricityPrice(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除电价信息")
    @DeleteMapping("/deleteElectricityPriceById")
    public ResponseResult<Integer> deleteElectricityPriceById(@RequestHeader("sysToken") String sysToken,@RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configElectricityPriceService.deleteElectricityPriceById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }



    @GetMapping("/getElectricityPriceById")
    @ApiOperation(value = "根据id查询电价详情")
    public ResponseResult<ConfigElectricityPriceDTO> getElectricityPriceById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigElectricityPriceDTO dto = configElectricityPriceService.getElectricityPriceById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


}

