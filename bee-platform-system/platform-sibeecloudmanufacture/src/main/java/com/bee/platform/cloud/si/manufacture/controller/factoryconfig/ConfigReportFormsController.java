package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigReportFormsDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigReportFormsEnableDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigReportFormsTreeDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigReportFormsUpdateStatusRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigReportFormsService;
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
 * 报表配置表 前端控制器
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */


@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "configReportForms", tags = "C-报表配置相关接口")
@RestController
@RequestMapping("/configReportForms")
public class ConfigReportFormsController {


    @Autowired
    private ConfigReportFormsService reportFormsService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchReportFormsList")
    @ApiOperation(value = "条件查询报表配置列表")
    public ResponseResult<List<ConfigReportFormsDTO>> searchReportFormsList(@RequestHeader("sysToken") String sysToken, String name, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return reportFormsService.searchReportFormsList(userInfo,name,page);
    }


    @GetMapping("/getEnableReportFormsList")
    @ApiOperation(value = "查询企业启用的报表配置列表")
    public ResponseResult<List<ConfigReportFormsEnableDTO>> getEnableReportFormsList(@RequestHeader("sysToken") String sysToken, @RequestParam String type){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigReportFormsEnableDTO> dto = reportFormsService.getEnableReportFormsList(userInfo,type);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "停用报表配置")
    @PostMapping("/stopReportForms")
    public ResponseResult<Integer> stopReportForms(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigReportFormsUpdateStatusRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = reportFormsService.stopReportForms(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "启用报表配置")
    @PostMapping("/enableReportForms")
    public ResponseResult<Integer> enableReportForms(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigReportFormsUpdateStatusRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = reportFormsService.enableReportForms(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }






    @GetMapping("/getReportFormsTree")
    @ApiOperation(value = "查询企业启用的报表配置完整---树")
    public ResponseResult<List<ConfigReportFormsTreeDTO>> getReportFormsTree(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigReportFormsTreeDTO> dto = reportFormsService.getReportFormsTree(userInfo);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }




}

