package com.bee.platform.cloud.si.manufacture.controller.factoryconfig;


import com.bee.platform.cloud.si.manufacture.dto.ConfigCodeDTO;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigCodeService;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * 码表 前端控制器
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "configCode", tags = "C-工厂系统配置码表相关接口")
@RequestMapping("/configCode")
public class ConfigCodeController {

    @Autowired
    private ConfigCodeService configCodeService;

    @GetMapping("/listConfigCodeByType")
    @ApiOperation(value = "根据类型查询码表列表")
    public ResponseResult<List<ConfigCodeDTO>> listConfigCodeByType(@RequestParam("type") String type){
        List<ConfigCodeDTO> list = configCodeService.listConfigCode(type);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }



}

