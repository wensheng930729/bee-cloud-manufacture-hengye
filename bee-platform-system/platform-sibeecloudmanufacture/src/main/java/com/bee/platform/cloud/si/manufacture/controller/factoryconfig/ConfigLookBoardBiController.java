package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigLookBoardBiDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigLookBoardBiEnableDTO;
import com.bee.platform.cloud.si.manufacture.dto.ConfigLookBoardBiTreeDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigLookBoardBiUpdateStatusRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigLookBoardBiService;
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
 * 看板BI配置表 前端控制器
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */

@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "configLookBoardBi", tags = "C-看板BI配置相关接口")
@RestController
@RequestMapping("/configLookBoardBi")
public class ConfigLookBoardBiController {



    @Autowired
    private ConfigLookBoardBiService lookBoardBiService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchLookBoardBiList")
    @ApiOperation(value = "条件查询看板BI配置列表")
    public ResponseResult<List<ConfigLookBoardBiDTO>> searchLookBoardBiList(@RequestHeader("sysToken") String sysToken, String name,Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return lookBoardBiService.searchLookBoardBiList(userInfo,name,page);
    }


    @GetMapping("/getEnableLookBoardBiList")
    @ApiOperation(value = "查询企业启用的看板BI配置列表")
    public ResponseResult<List<ConfigLookBoardBiEnableDTO>> getEnableLookBoardBiList(@RequestHeader("sysToken") String sysToken, @RequestParam String type){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigLookBoardBiEnableDTO> dto = lookBoardBiService.getEnableLookBoardBiList(userInfo,type);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "停用看板BI配置")
    @PostMapping("/stopLookBoardBi")
    public ResponseResult<Integer> stopLookBoardBi(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigLookBoardBiUpdateStatusRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = lookBoardBiService.stopLookBoardBi(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "启用看板BI配置")
    @PostMapping("/enableLookBoardBi")
    public ResponseResult<Integer> enableLookBoardBi(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigLookBoardBiUpdateStatusRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = lookBoardBiService.enableLookBoardBi(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }




    @GetMapping("/getLookBoardBiTree")
    @ApiOperation(value = "查询企业启用的看板BI配置完整---树")
    public ResponseResult<List<ConfigLookBoardBiTreeDTO>> getLookBoardBiTree(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigLookBoardBiTreeDTO> dto = lookBoardBiService.getLookBoardBiTree(userInfo);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}

