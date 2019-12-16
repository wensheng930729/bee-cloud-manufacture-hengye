package com.bee.platform.cloud.si.manufacture.controller;


import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumWeightMachine;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyLogisticsBatchService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyWeightMachineService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleWeightMachineService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.sun.javafx.collections.MappingChange;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.math.BigDecimal;
import java.util.*;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅数据信息 前端控制器
 * @Date 2019/9/23 18:47
 */
@Api(value = "地磅相关接口", tags = "地磅相关接口-API")
@RestController
@RequestMapping("/weightMachine")
@CrossOrigin("*")
@Slf4j
public class WeightMachineController {

    @Autowired
    private BuyWeightMachineService buyWeightMachineService;
    @Autowired
    private SaleWeightMachineService saleWeightMachineService;
    @Autowired
    private BuyLogisticsBatchService buyLogisticsBatchService;
    @Autowired
    private UserInfoUtils userInfoUtils;


    /**
     * ******************************************** 采购 *********************************************
     **/

    @PostMapping("/saveBuyWeightMachine")
    @ApiOperation(value = "保存确认地磅数据信息（采购）", notes = "保存确认地磅数据信息")
    public ResponseResult<String> saveBuyWeightMachine(@RequestHeader("sysToken") String sysToken, @RequestBody WeightMachineRq weightMachineRq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyWeightMachineService.saveBuyWeightMachine(weightMachineRq, userInfo, EnumWeightMachine.DataSource.ADD.getValue());
    }


    @GetMapping("/getWeitghtIdByProductId/{productId}")
    @ApiOperation(value = "多车一样界面根据产品id查询未完成合同的磅单号下拉列表", notes = "多车一样界面根据产品id查询未完成合同的磅单号下拉列表 ")
    public ResponseResult<List<BuyWeightMachineBoxDTO>> getWeitghtIdByProductId(@RequestHeader("sysToken") String sysToken, @PathVariable("productId") Integer productId) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyWeightMachineService.getWeitghtIdByProductId(productId, userInfo);
    }


    /**
     * ******************************************** 销售 *********************************************
     **/

    @PostMapping("/saveSaleWeightMachine")
    @ApiOperation(value = "保存确认地磅数据信息（销售）", notes = "保存确认地磅数据信息")
    public ResponseResult<String> saveSaleWeightMachine(@RequestHeader("sysToken") String sysToken, @RequestBody WeightMachineRq weightMachineRq) {
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return saleWeightMachineService.saveSaleWeightMachine(weightMachineRq, userInfo, EnumWeightMachine.DataSource.ADD.getValue());
    }


    /**
     * ******************************************** 公共 *********************************************
     **/
    @GetMapping("/getWeightMachineList")
    @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售")
    @ApiOperation(value = "获取未称重磅单列表信息", notes = "获取未称重磅单列表信息 ")
    public ResponseResult<List<WeightMachineDTO>> getWeightMachineList(@RequestHeader("sysToken") String sysToken, Integer type, Page page) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            List<WeightMachineDTO> buyWeightMachineDTOS =
                    buyWeightMachineService.getBuyWeightMachineList(userInfo.getOrgId(), pagination);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, buyWeightMachineDTOS, PageUtils.transToPage(pagination));
        }

        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            List<WeightMachineDTO> buyWeightMachineDTOS =
                    saleWeightMachineService.getSaleWeightMachineList(userInfo.getOrgId(), pagination);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, buyWeightMachineDTOS, PageUtils.transToPage(pagination));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }


    @GetMapping("/getWeightMachineListTotal")
    @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售", required = true)
    @ApiOperation(value = "获取已称重磅单列表信息（总览）", notes = "获取已称重磅单列表信息（总览） ")
    public ResponseResult<List<WeightMachineTotalDTO>> getWeightMachineListTotal(@RequestHeader("sysToken") String sysToken, Integer type, Page page) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            List<WeightMachineTotalDTO> weightMachineListTotals =
                    buyWeightMachineService.getWeightMachineListTotal(userInfo.getOrgId(), pagination);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, weightMachineListTotals, PageUtils.transToPage(pagination));
        }

        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            List<WeightMachineTotalDTO> weightMachineListTotals =
                    saleWeightMachineService.getWeightMachineListTotal(userInfo.getOrgId(), pagination);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, weightMachineListTotals, PageUtils.transToPage(pagination));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }

    @GetMapping("/getWeightMachineListDetails")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售", required = true),
            @ApiImplicitParam(name = "contractNum", value = "合同编号", required = true)}
    )
    @ApiOperation(value = "获取已称重磅单列表信息（详细信息）", notes = "获取已称重磅单列表信息（详细信息） ")
    public ResponseResult<List<WeightMachineDTO>> getWeightMachineListDetails(@RequestHeader("sysToken") String sysToken,
                                                                              Integer type, String contractNum, Page page) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            List<WeightMachineDTO> weightMachineDetailsDTOs =
                    buyWeightMachineService.getWeightMachineListDetails(userInfo.getOrgId(), contractNum, pagination);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, weightMachineDetailsDTOs, PageUtils.transToPage(pagination));
        }

        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            List<WeightMachineDTO> weightMachineDetailsDTOs =
                    saleWeightMachineService.getWeightMachineListDetails(userInfo.getOrgId(), contractNum, pagination);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, weightMachineDetailsDTOs, PageUtils.transToPage(pagination));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }


    @PostMapping("/confirmWeight")
    @ApiOperation(value = "确认进厂/出厂称重", notes = "确认进厂/出厂称重")
    public ResponseResult<String> confirmWeight(@RequestHeader("sysToken") String sysToken, @RequestBody() @Valid ConfirmWeightMachineRq confirmWeightMachineRq) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer weightType = confirmWeightMachineRq.getWeightType();
        //采购
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(weightType)) {
            return buyWeightMachineService.buyConfirmWeight(confirmWeightMachineRq, userInfo);
        }
        //销售
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(weightType)) {
            return saleWeightMachineService.saleConfirmWeight(confirmWeightMachineRq, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.CONFIRM_FAILED);
    }


    @GetMapping("/samplePush")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售", required = true),
            @ApiImplicitParam(name = "machineId", value = "磅单编号", required = true)}
    )
    @ApiOperation(value = "通知取样", notes = "通知取样 ")
    public ResponseResult<String> samplePush(@RequestHeader("sysToken") String sysToken, Integer type, String machineId) {

        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        //采购
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.samplePush(machineId, userInfo);
        }
        //销售
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.samplePush(machineId, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.CONFIRM_FAILED);
    }


    @GetMapping("/getWeight")
    @ApiImplicitParam(name = "deviceId", value = "磅单设备编号", required = true)
    @ApiOperation(value = "开始称重（进厂称重/出厂称重 ... ）", notes = "开始称重（进厂称重/出厂称重 ... ） ")
    public ResponseResult<String> getWeight(String deviceId) {
        log.info("设备id：{}", deviceId);
        String inData = buyWeightMachineService.getWeight(deviceId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, null, inData);
    }


    @PostMapping("/confirmMachine")
    @ApiOperation(value = "结束磅单", notes = "结束磅单")
    public ResponseResult<String> confirmMachine(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfirmMachineRQ confirmMachineRQ) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer type = confirmMachineRQ.getType();
        //销售
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.confirmMachine(confirmMachineRQ, userInfo);
        }
        //采购
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.confirmMachine(confirmMachineRQ, userInfo);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }

    @PostMapping("/continueMachine")
    @ApiOperation(value = "继续运载", notes = "继续运载")
    public ResponseResult<String> continueMachine(@RequestHeader("sysToken") String sysToken, @RequestBody @Valid ConfirmMachineRQ confirmMachineRQ) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer type = confirmMachineRQ.getType();
        //采购
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.continueMachine(confirmMachineRQ, userInfo);
        }
        //销售
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.continueMachine(confirmMachineRQ, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }

    @PostMapping("/delMachine")
    @ApiOperation(value = "删除磅单", notes = "删除磅单")
    public ResponseResult<String> delMachine(@RequestHeader("sysToken") String sysToken, @RequestBody ConfirmMachineRQ confirmMachineRQ) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer type = confirmMachineRQ.getType();
        //销售
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.delMachine(confirmMachineRQ, userInfo);
        }
        //采购
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.delMachine(confirmMachineRQ, userInfo);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }


    /**
     * @Description 六个小时执行一次删除数据任务  //  0 0 0/6 * * ?
     * @author chenxm66777123
     * @Date 2019/10/21 9:19
     * @version 1.0.0
     */
    @Scheduled(cron = "0 0 0/6 * * ?")
    @Transactional(rollbackFor = Exception.class)
    public void clearWeightData() {
        buyWeightMachineService.clearWeightData();
        log.info("数据删除成功{}", new Date());
    }

    @GetMapping("/getMachineByTrainNum")
    @ApiOperation(value = "多车一样界面-根据车牌号查询合同磅单信息", notes = "多车一样界面-根据车牌号查询合同磅单信息")
    public ResponseResult<SampleMachineBuyDTO> getMachineByTrainNum(@RequestHeader("sysToken") String sysToken
            , @RequestParam(required = true) String trainNumber) {
        if (StringUtils.isBlank(trainNumber)) {
            log.error("参数不正确，请传入车牌号，车牌号：{}", trainNumber);
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        // 获取当前用户信息
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyWeightMachineService.getMachineByTrainNum(trainNumber, userInfo);
    }

    @GetMapping("/getSampleMachineBox")
    @ApiOperation(value = "多车一样界面-查询从未取样的磅单车牌号下拉列表", notes = "多车一样界面-查询从未取样的磅单车牌号下拉列表")
    public ResponseResult<List<SampleMachineBuyDTO>> getSampleMachineBox(@RequestHeader("sysToken") String sysToken,
                                                                         Integer productId) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return buyWeightMachineService.getSampleMachineBox(productId, userInfo);
    }

    @GetMapping("/confirmDeductWeight")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售", required = true),
            @ApiImplicitParam(name = "machineId", value = "磅单编号", required = true),
            @ApiImplicitParam(name = "deductWeight", value = "扣重", required = true),
            @ApiImplicitParam(name = "deductWeightByManual", value = "扣重显示按钮 0 确认扣重按钮 2 手动修改按钮", required = true)
    }
    )
    @ApiOperation(value = "确认扣重", notes = "确认扣重")
    public ResponseResult<String> confirmDeductWeight(@RequestHeader("sysToken") String sysToken,
                                                      Integer type, String machineId, BigDecimal deductWeight, Integer deductWeightByManual) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        //销售
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.confirmDeductWeight(machineId, deductWeight, deductWeightByManual, userInfo);
        }
        //采购
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.confirmDeductWeight(machineId, deductWeight, deductWeightByManual, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.CONFIRM_FAILED);
    }

    @GetMapping("/getLastCarrierByTrainNumber")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售", required = true),
            @ApiImplicitParam(name = "trainNumber", value = "车牌号", required = true)
    }
    )
    @ApiOperation(value = "根据车牌号，自动带出最近一次该车所对应的承运商（未找到返回失败）", notes = "根据车牌号，自动带出最近一次该车所对应的承运商。（未找到返回失败）")
    public ResponseResult<CarrierInfoDTO> getLastCarrierByCarNumber(@RequestHeader("sysToken") String sysToken, Integer type, String trainNumber) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        //采购
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.getLastCarrierByTrainNumber(trainNumber, userInfo);
        }
        //销售
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.getLastCarrierByTrainNumber(trainNumber, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
    }

    @GetMapping("/getWeighListInfo")
    @ApiOperation(value = "查询过磅单数据信息", notes = "查询过磅单数据信息 ")
    public ResponseResult<List<WeighingListDTO>> getWeighListInfo(@RequestHeader("sysToken") String sysToken,
                                                                  WeighingListRq rq, Page page) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                buyWeightMachineService.getWeighListInfo(rq, userInfo, pagination), PageUtils.transToPage(pagination));
    }


    @GetMapping("/saveRemark")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售", required = true),
            @ApiImplicitParam(name = "machineId", value = "磅单编号", required = true),
            @ApiImplicitParam(name = "remark", value = "备注")
    }
    )
    @ApiOperation(value = "保存备注", notes = "保存备注")
    public ResponseResult<String> saveRemark(@RequestHeader("sysToken") String sysToken,
                                                      Integer type, String machineId, String remark) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        //销售
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.saveRemark(machineId, remark, userInfo);
        }
        //采购
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.saveRemark(machineId, remark, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.CONFIRM_FAILED);
    }


    @GetMapping("/updateTrainNumber")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售",required = true),
            @ApiImplicitParam(name = "trainNumber", value = "车牌号",required = true),
            @ApiImplicitParam(name = "machineId", value = "磅单id",required = true)
    }
    )
    @ApiOperation(value = "修改车牌号", notes = "修改车牌号")
    public ResponseResult<String> updateTrainNumber(@RequestHeader("sysToken") String sysToken,Integer type,String machineId,String trainNumber) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.updateTrainNumber(machineId,trainNumber,userInfo);
        }
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.updateTrainNumber(machineId,trainNumber,userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }



    /**
     * ******************************************** Web端 *********************************************
     **/

    @GetMapping("/getWeightMachineWebCount")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售",required = true),
            @ApiImplicitParam(name = "isWeight", value = "是否过磅 0 待过磅车辆 1 已过磅车辆",required = true)
      }
    )
    @ApiOperation(value = "web端地磅头部车辆、净重统计", notes = "web端地磅头部车辆、净重统计")
    public ResponseResult<WeightMachineWebCountDTO> getWeightMachineWebCount(@RequestHeader("sysToken") String sysToken, Integer type,Integer isWeight) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.getWeightMachineWebCount(isWeight, userInfo);
        }
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.getWeightMachineWebCount(isWeight, userInfo);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }



    @PostMapping("/getWeightMachineWebList")
    @ApiOperation(value = "web获取待过磅/已过磅列表信息", notes = "web获取待过磅/已过磅列表信息")
    public ResponseResult<List<WeightMachineWebDTO>> getWeightMachineWebList(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid WeightMachineWebListRq rq, Page page) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        Integer type = rq.getType();
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            List<WeightMachineWebDTO> weightMachineWebList =
                    buyWeightMachineService.getWeightMachineWebList(rq,userInfo, pagination);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, weightMachineWebList, PageUtils.transToPage(pagination));
        }

        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            List<WeightMachineWebDTO> weightMachineWebList =
                   saleWeightMachineService.getWeightMachineWebList(rq,userInfo, pagination);
           return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, weightMachineWebList, PageUtils.transToPage(pagination));
       }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }


    @GetMapping("/getWeightMachineWebDeatil")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "磅单类型 1 采购 2 销售",required = true),
            @ApiImplicitParam(name = "machineId", value = "磅单id",required = true)
    }
    )
    @ApiOperation(value = "web根据磅单号获取详细信息", notes = "web根据磅单号获取详细信息")
    public ResponseResult<WeightMachineWebDTO> getWeightMachineWebDeatil(@RequestHeader("sysToken") String sysToken,Integer type,String machineId) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.getWeightMachineWebDeatil(machineId);
        }
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.getWeightMachineWebDeatil(machineId);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
    }


    /**
     * ******************************************** Web端磅单绑定 *********************************************
     **/



    @PostMapping("/getWeightMachineWebBindList")
    @ApiOperation(value = "web磅单绑定合同号列表查询", notes = "web磅单绑定合同号列表查询")
    public ResponseResult<List<WeightMachineWebBindDTO>> getWeightMachineWebBindList(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid WeightMachineBindRq rq, Page page) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);

        List<WeightMachineWebBindDTO> weightMachineWebList =
                buyWeightMachineService.getWeightMachineWebBindList(rq,userInfo,pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, weightMachineWebList, PageUtils.transToPage(pagination));

    }

    @GetMapping("/getRelationContract")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "custOrSupName", value = "客户/供应商",required = true),
            @ApiImplicitParam(name = "productName", value = "产品名称",required = true)
    }
    )
    @ApiOperation(value = "web磅单获取管理的合同号", notes = "web磅单获取管理的合同号")
    public ResponseResult<List<WeightMachineWebRelationContractDTO>> getRelationContract(@RequestHeader("sysToken") String sysToken
            ,String custOrSupName,String productName,Page page) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);

        List<WeightMachineWebRelationContractDTO> weightMachineWebRelationContractDTOS =
                buyWeightMachineService.getRelationContract(userInfo,custOrSupName,productName,pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, weightMachineWebRelationContractDTOS, PageUtils.transToPage(pagination));

    }

    @PostMapping("/ignoreRecoveryMachine")
    @ApiOperation(value = "web磅单忽略/恢复", notes = "web磅单忽略/恢复")
    public ResponseResult<String> ignoreRecoveryMachine(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid WeightIgnoreMachineRq rq) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer type = rq.getType();
        if (EnumWeightMachine.WeightType.BUY.getValue().equals(type)) {
            return buyWeightMachineService.ignoreRecoveryMachine(rq);
        }
        if (EnumWeightMachine.WeightType.SALE.getValue().equals(type)) {
            return saleWeightMachineService.ignoreRecoveryMachine(rq);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_SUBMIT);

    }

    @PostMapping("/machineBindContract")
    @ApiOperation(value = "web磅单绑定合同号", notes = "web磅单绑定合同号")
    public ResponseResult<String> machineBindContract(@RequestHeader("sysToken") String sysToken,@RequestBody @Valid List<WeightMachineBindContractRq> rq) {
        // 获取当前用户信
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<Map<String,Object>> buyParamList = new ArrayList<>();
        List<Map<String,Object>> saleParamList = new ArrayList<>();
        rq.stream().forEach(obj->{
            Map<String,Object> map  = new HashMap<>();
            map.put("contractBusinessId",obj.getContractBusinessId());
            map.put("machineId",obj.getMachineId());
            map.put("contractNum",obj.getContractNum());
            map.put("userId",userInfo.getId());
            map.put("userName",userInfo.getName());
            if (EnumWeightMachine.WeightType.BUY.getValue().equals(obj.getType())) {
                //采购，查询合同的第一个批次号
                map.put("batchId", buyLogisticsBatchService.getContractLogisticsBatchId(obj.getContractBusinessId()));
                buyParamList.add(map);
            }
            if (EnumWeightMachine.WeightType.SALE.getValue().equals(obj.getType())) {
                saleParamList.add(map);
            }
        });

        if(!CollectionUtils.isEmpty(buyParamList)){
            buyWeightMachineService.batchBindContract(buyParamList);
        }
        if(!CollectionUtils.isEmpty(saleParamList)){
            saleWeightMachineService.batchBindContract(saleParamList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


}

