package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigRepositoryDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRepositorySaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigRepositoryUpdateRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigSearchTypeListRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRepositoryService;
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
 * 仓库档案 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configRepository", tags = "C-仓库管理相关接口")
@RequestMapping("/configRepository")
public class ConfigRepositoryController {



    @Autowired
    private ConfigRepositoryService configRepositoryService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchRepositoryList")
    @ApiOperation(value = "条件查询仓库列表")
    public ResponseResult<List<ConfigRepositoryDTO>> searchRepositoryList(@RequestHeader("sysToken") String sysToken,String name, Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return configRepositoryService.searchRepositoryList(userInfo,name,page);
    }


    @ApiOperation(value = "保存仓库信息")
    @PostMapping("/saveRepository")
    public ResponseResult<Integer> saveRepository(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigRepositorySaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configRepositoryService.saveRepository(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改仓库信息")
    @PostMapping("/updateRepository")
    public ResponseResult<Integer> updateRepository(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigRepositoryUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = configRepositoryService.updateRepository(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除仓库信息")
    @DeleteMapping("/deleteRepositoryById")
    public ResponseResult<Integer> deleteRepositoryById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        configRepositoryService.deleteRepositoryById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }




    @PostMapping("/getRepositoryListByType")
    @ApiOperation(value = "根据类别查询仓库列表(仓库类别(0 成品 1 原料 2 配料  3五金 4其他) 空为查询全部)")
    public ResponseResult<List<ConfigRepositoryDTO>> getRepositoryListByType(@RequestHeader("sysToken") String sysToken,@RequestBody ConfigSearchTypeListRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ConfigRepositoryDTO> dto = configRepositoryService.getRepositoryListByType(userInfo,rq.getTypes());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }



    @GetMapping("/getRepositoryById")
    @ApiOperation(value = "根据id查询仓库详情")
    public ResponseResult<ConfigRepositoryDTO> getRepositoryById(@RequestHeader("sysToken") String sysToken, @RequestParam("id") Integer id){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        ConfigRepositoryDTO dto = configRepositoryService.getRepositoryById(userInfo,id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

}

