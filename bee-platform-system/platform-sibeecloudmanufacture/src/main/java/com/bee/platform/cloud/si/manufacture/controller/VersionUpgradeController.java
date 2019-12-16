package com.bee.platform.cloud.si.manufacture.controller;


import com.bee.platform.cloud.si.manufacture.dto.VersionUpgradeDTO;
import com.bee.platform.cloud.si.manufacture.service.VersionUpgradeService;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.entity.ResponseResult;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * <p>
 * 版本升级描述 前端控制器
 * </p>
 *
 * @author LL123
 * @since 2019-11-25
 */
@RestController
@RequestMapping("/versionUpgrade")
@Api(value = "版本升级描述相关接口", tags = "版本升级描述相关接口")
public class VersionUpgradeController {

    @Autowired
    private VersionUpgradeService versionUpgradeService;

    @NotIntercept
    @GetMapping(value = "/getByVersionNum")
    @ApiOperation(value = "根据版本号查询升级描述", notes = "根据版本号查询升级描述")
    public ResponseResult<List<VersionUpgradeDTO>> getByVersionNum(String versionNum) {
        return versionUpgradeService.getByVersionNum(versionNum);
    }

}

