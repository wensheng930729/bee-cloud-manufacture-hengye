package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigProductSpecDTO;
import com.bee.platform.cloud.si.manufacture.rq.ConfigProductSpecUpdateRQ;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigProductSpecService;
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
 * 产品规格表 前端控制器
 * </p>
 *
 * @author MP123
 * @since 2019-10-15
 */


@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "configProductSpec", tags = "C-产品规格相关接口")
@RestController
@RequestMapping("/configProductSpec")
public class ConfigProductSpecController {



    @Autowired
    private ConfigProductSpecService productSpecService;

    @Autowired
    private UserInfoUtils userInfoUtils;



    @ApiOperation(value = "修改产品规格配置信息")
    @PostMapping("/updateProductSpec")
    public ResponseResult updateProductSpec(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfigProductSpecUpdateRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

         productSpecService.updateProductSpec(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);

    }


    @ApiOperation(value = "根据产品id查询产品规格列表")
    @GetMapping("/getProductSpecByProductId/{productId}")
    public ResponseResult<List<ConfigProductSpecDTO>> getProductSpecByProductId(@RequestHeader("sysToken") String sysToken, @PathVariable(value = "productId") Integer productId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        if (ObjectUtils.isEmpty(productId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(userInfo)) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<ConfigProductSpecDTO> dto = productSpecService.getProductSpecByProductId(productId,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto );
    }

}

