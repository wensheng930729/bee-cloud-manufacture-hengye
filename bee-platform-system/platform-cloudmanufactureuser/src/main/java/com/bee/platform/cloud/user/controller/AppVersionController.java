package com.bee.platform.cloud.user.controller;

import com.bee.platform.cloud.user.dto.AppVersionDTO;
import com.bee.platform.cloud.user.service.AuthAppVersionService;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.entity.ResponseResult;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-09-30 09:07
 **/
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "app版本号相关的接口", tags = "app版本号相关的接口")
@RequestMapping("/auth/client")
public class AppVersionController {

    @Autowired
    private AuthAppVersionService authAppVersionService;

    @NotIntercept
    @ApiOperation(value = "获取app最新的版本号", notes = "获取app最新的版本号")
    @GetMapping(value = "/getVersion")
    public ResponseResult<List<AppVersionDTO>> getVersion() {
        return authAppVersionService.getVersion();
    }
}
