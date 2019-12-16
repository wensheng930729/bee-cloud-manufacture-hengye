package com.bee.platform.cloud.si.manufacture.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.FieldTypeDTO;
import com.bee.platform.cloud.si.manufacture.dto.PlcFactoryGatewayDTO;
import com.bee.platform.cloud.si.manufacture.dto.PlcFieldConfigDTO;
import com.bee.platform.cloud.si.manufacture.rq.FactoryGatewayParamRQ;
import com.bee.platform.cloud.si.manufacture.rq.FactoryGatewayRQ;
import com.bee.platform.cloud.si.manufacture.rq.PlcInfoRQ;
import com.bee.platform.cloud.si.manufacture.service.PlcFactoryGatewayService;
import com.bee.platform.cloud.si.manufacture.service.PlcFieldConfigService;
import com.bee.platform.common.constants.enums.FieldType;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.Arrays;
import java.util.List;
import java.util.Map;


/**
 * <p>
 * 打印机相关接口
 * </p>
 *@author
 *@date
 */
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/plc/business")
@Api(value = "plc", tags = "PLC -- 相关的网关配置和字段配置以及数据获取的接口")
public class PlcDataController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private PlcFactoryGatewayService factoryGatewayService;

    @Autowired
    private PlcFieldConfigService plcFieldConfigService;

    @GetMapping("/gateways")
    @ApiOperation(value = "获取当前工厂配置的华辰智通PLC数据采集网关列表")
    public ResponseResult<List<PlcFactoryGatewayDTO>> getFactoryGateways(@RequestHeader("sysToken")String sysToken,
                                                                         FactoryGatewayParamRQ rq, Page page){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        Pagination pagination= PageUtils.transFromPage(page);
        return factoryGatewayService.getFactoryGateways(userInfo.getFactoryId(),rq,pagination);
    }

    @PostMapping("/gateway")
    @ApiOperation(value = "新增网关-华辰智通PLC数据采集网关")
    public ResponseResult<ResCodeEnum> addFactoryGateways(@RequestHeader("sysToken")String sysToken,
                                                          @RequestBody @Valid FactoryGatewayRQ rq){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        return factoryGatewayService.addFactoryGateways(userInfo,rq);
    }

    @PutMapping("/gateway/{hcGatewayId}")
    @ApiOperation(value = "禁用或启用PLC数据采集网关")
    public ResponseResult<ResCodeEnum> updateFactoryGateways(@RequestHeader("sysToken")String sysToken,
                                                          @PathVariable("hcGatewayId") String hcGatewayId){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        return factoryGatewayService.updateFactoryGateways(userInfo,hcGatewayId);
    }

    @DeleteMapping("/gateway/{hcGatewayId}")
    @ApiOperation(value = "删除PLC数据采集网关")
    public ResponseResult<ResCodeEnum> deleteFactoryGateways(@RequestHeader("sysToken")String sysToken,
                                                             @PathVariable("hcGatewayId") String hcGatewayId){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        return factoryGatewayService.deleteFactoryGateways(userInfo,hcGatewayId);
    }


    @GetMapping("/fieldTypes")
    @ApiOperation(value = "获取所有的PLC下料斗类型")
    public ResponseResult<List<FieldTypeDTO>> getFieldTypes(){
        return plcFieldConfigService.getFieldTypes();
    }


    @GetMapping("/fields")
    @ApiOperation(value = "获取当前PLC配置的下料斗")
    public ResponseResult<List<PlcFieldConfigDTO>> getPlcFields(@RequestHeader("sysToken")String sysToken,
                                                                Integer plcId){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        if(plcId==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.PLC_ID_NOT_NULL);
        }
        return plcFieldConfigService.getPlcFields(userInfo,plcId);
    }
}

