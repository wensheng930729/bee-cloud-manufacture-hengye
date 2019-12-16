package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigTestAttributeDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigSearchTypeListRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigTestAttributeSaveRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigTestAttributeSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigTestAttributeUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigTestAttributeService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
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
 * 化验属性配置表 前端控制器
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */

@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "configTestAttribute", tags = "C-化验属性配置相关接口")
@RestController
@RequestMapping("/configTestAttribute")
public class ConfigTestAttributeController {

    @Autowired
    private ConfigTestAttributeService testAttributeService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/searchTestAttributeList")
    @ApiOperation(value = "条件查询化验属性列表")
    public ResponseResult<List<ConfigTestAttributeDTO>> searchTestAttributeList(@RequestHeader("sysToken") String sysToken, ConfigTestAttributeSearchRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        List<ConfigTestAttributeDTO> dto= testAttributeService.searchTestAttributeList(userInfo,rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @PostMapping("/getTestAttributeByType")
    @ApiOperation(value = "根据类型查询化验属性列表（0 输入项 1 输出项）空为查全部")
    public ResponseResult<List<ConfigTestAttributeDTO>> getTestAttributeByType(@RequestHeader("sysToken") String sysToken,@RequestBody ConfigSearchTypeListRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigTestAttributeDTO> dto= testAttributeService.getTestAttributeByType(userInfo,rq.getTypes());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }



    @ApiOperation(value = "保存化验属性")
    @PostMapping("/saveTestAttribute")
    public ResponseResult<Integer> saveTestAttribute(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigTestAttributeSaveRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = testAttributeService.saveTestAttribute(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "修改化验属性")
    @PostMapping("/updateTestAttribute")
    public ResponseResult<Integer> updateTestAttribute(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigTestAttributeUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = testAttributeService.updateTestAttribute(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @ApiOperation(value = "根据id删除化验属性")
    @DeleteMapping("/deleteTestAttributeById/{id}")
    public ResponseResult<Integer> deleteTestAttributeById(@RequestHeader("sysToken") String sysToken, @PathVariable("id") Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        testAttributeService.deleteTestAttributeById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


}

