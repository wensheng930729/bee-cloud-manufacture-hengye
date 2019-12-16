package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigAmmeterDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigAmmeterSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigAmmeterUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigAmmeterService;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.utils.CustomerInfoUtils;
import com.bee.platform.common.utils.SupplierInfoUtils;
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
 * 电表档案 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "configAmmeter", tags = "C-电表管理相关接口")
@RestController
@RequestMapping("/configAmmeter")
public class ConfigAmmeterController {

    @Autowired
    private ConfigAmmeterService configAmmeterService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private CustomerInfoUtils customerInfoUtils;

    @Autowired
    private SupplierInfoUtils supplierInfoUtils;

    @GetMapping("/searchAmmeterList")
    @ApiOperation(value = "条件查询电表列表")
    public ResponseResult<List<ConfigAmmeterDTO>> searchAmmeterList(@RequestHeader("sysToken") String sysToken,String ammeterName, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        return configAmmeterService.searchAmmeterList(userInfo,ammeterName,page);
    }


    @ApiOperation(value = "保存电表信息")
    @PostMapping("/saveAmmeter")
    public ResponseResult<Integer> saveAmmeter(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigAmmeterSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configAmmeterService.saveAmmeter(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改电表信息")
    @PostMapping("/updateAmmeter")
    public ResponseResult<Integer> updateAmmeter(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigAmmeterUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configAmmeterService.updateAmmeter(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除电表信息")
    @DeleteMapping("/deleteAmmeterById")
    public ResponseResult<Integer> deleteAmmeterById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configAmmeterService.deleteAmmeterById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }



    @GetMapping("/getAmmeterById")
    @ApiOperation(value = "根据id查询电表详情")
    public ResponseResult<ConfigAmmeterDTO> getAmmeterById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigAmmeterDTO dto = configAmmeterService.getAmmeterById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @GetMapping("/getAmmeterList")
    @ApiOperation(value = "查询用户公司下的电表列表")
    public ResponseResult<List<ConfigAmmeterDTO>> getAmmeterList(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigAmmeterDTO> dto =  configAmmeterService.getAmmeterList(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}

