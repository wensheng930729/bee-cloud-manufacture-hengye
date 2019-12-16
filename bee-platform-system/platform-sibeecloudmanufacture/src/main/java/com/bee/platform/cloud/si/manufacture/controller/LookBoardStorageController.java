package com.bee.platform.cloud.si.manufacture.controller;

import com.bee.platform.cloud.si.manufacture.dto.LookBoarOutStorageDTO;
import com.bee.platform.cloud.si.manufacture.dto.RawMaterialDTO;
import com.bee.platform.cloud.si.manufacture.service.LookBoardStorageService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @ClassName: LookBoardStorageController
 * @Description: 管理后台数据看板库存情况
 * @Author: fei.sun
 * @Date: 2019/10/18 16:07
 * @Version: 1.0
 */

@RestController
@RequestMapping(value = "/storage")
@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "管理后台数据看板库存情况相关接口", tags = "管理后台数据看板库存情况相关接口")
public class LookBoardStorageController {

    private final LookBoardStorageService lookBoardStorageService;

    private final UserInfoUtils userInfoUtils;

    @Autowired
    public LookBoardStorageController(LookBoardStorageService lookBoardStorageService,UserInfoUtils userInfoUtils){
        this.lookBoardStorageService = lookBoardStorageService;
        this.userInfoUtils = userInfoUtils;
    }

    @ApiOperation("原料库存情况和成品库存")
    @GetMapping("/selectInStorage")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "goodsType",value = "原料类型，1 主料 2 辅料 3 其他 4 成品",required = true),
            @ApiImplicitParam(name = "dateTime",value = "选择查询的日期，格式：yyyy-MM-dd",required = true)
    })
    public ResponseResult<List<RawMaterialDTO>> selectStock(@RequestHeader("sysToken") String sysToken,Integer goodsType,String dateTime){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        List<RawMaterialDTO> rawMaterialDTOS = lookBoardStorageService.selectRawMaterial(goodsType,dateTime,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,rawMaterialDTOS);
    }

    @ApiOperation("出库情况")
    @GetMapping("/selectOutStorage")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type",value = "查询类型，1 按日 2 按月",required = true),
            @ApiImplicitParam(name = "goodsType",value = "产品类型，1 主料 2 辅料 3 其他 4 成品",required = true),
            @ApiImplicitParam(name = "startTime",value = "起始时间,格式 yyyy-MM-dd",required = true),
            @ApiImplicitParam(name = "endTime",value = "终止时间,格式 yyyy-MM-dd",required = true)
    })
    public ResponseResult<List<LookBoarOutStorageDTO>> selectOutStorage(@RequestHeader("sysToken") String sysToken, Integer type
            , Integer goodsType,String startTime, String endTime){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        List<LookBoarOutStorageDTO> lookBoarOutStorageDTOS = lookBoardStorageService.selectOutStorage(type,goodsType,
                startTime,endTime,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,lookBoarOutStorageDTOS);
    }

    @ApiOperation("在途量")
    @GetMapping("/selectInTransit")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "goodsType",value = "原料类型，1 主料 2 辅料 3 其他",required = true)
    })
    public ResponseResult<List<RawMaterialDTO>> selectInTransit(@RequestHeader("sysToken") String sysToken,Integer goodsType){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        List<RawMaterialDTO> rawMaterialDTOS = lookBoardStorageService.selectInTransit(goodsType,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,rawMaterialDTOS);
    }

}
