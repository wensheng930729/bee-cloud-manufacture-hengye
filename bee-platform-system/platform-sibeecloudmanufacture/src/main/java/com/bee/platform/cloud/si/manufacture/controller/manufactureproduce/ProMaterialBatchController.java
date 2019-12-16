package com.bee.platform.cloud.si.manufacture.controller.manufactureproduce;


import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.ProDeviceInspection;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.StorageInventoryService;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * <p>
 * 料批基本信息主表 前端控制器
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-24
 */
@RestController
@RequestMapping("/proMaterialBatch")
@CrossOrigin(origins = "*")
@Api(value = "ProMaterialBatchController",tags = "工艺主管相关接口")
public class ProMaterialBatchController {

    @Autowired
    private ProMaterialBatchService materialBatchService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ProIngredientService ingredientService;

    @Autowired
    private ProBlankingService blankingService;

    @Autowired
    private ProArtificialFeedService artificialFeedService;

    @Autowired
    private ProDeviceInspectionService deviceInspectionService;

    @Autowired
    private StorageInventoryService storageInventoryService;

    @Autowired
    private ProOreFurnaceRecordService oreFurnaceRecordService;

    @Autowired
    private ProOreFurnaceSampleService oreFurnaceSampleService;

    @PostMapping("/addMaterial")
    @ApiOperation(value = "新增料批")
    public ResponseResult addMaterialBatch(@RequestHeader("sysToken") String sysToken, @RequestBody MaterialBatchRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return materialBatchService.addMaterialBatch(rq, userInfo);
    }

    @PostMapping("/updateMaterial")
    @ApiOperation(value = "更新料批")
    public ResponseResult updateMaterialBatch(@RequestHeader("sysToken") String sysToken, @RequestBody MaterialBatchRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return materialBatchService.updateMaterialBatch(rq, userInfo);
    }

    @GetMapping("/getMaterial")
    @ApiOperation(value = "查询料批详情")
    public ResponseResult<MaterialBatchDTO> findInfo(@RequestParam Long id) {
        return materialBatchService.findInfo(id);
    }

    @GetMapping("/getMaterialByPlc")
    @ApiOperation(value = "查询plc料批详情")
    public ResponseResult<MaterialBatchDTO> findInfoByPlcId(@RequestParam Integer plcId) {
        return materialBatchService.findInfoByPlcId(plcId);
    }

    @PostMapping("/list/materials")
    @ApiOperation(value = "查询料批列表")
    public ResponseResult<List<MaterialBatchDTO>> findList(@RequestHeader("sysToken") String sysToken, @RequestBody MaterialQueryRQ rq, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return materialBatchService.findList(userInfo, rq, pagination);
    }

    @GetMapping("/comboBox/materials")
    @ApiOperation(value = "查询料批下拉列表")
    public ResponseResult<List<MaterialBatchDTO>> findComboBoxList(@RequestHeader("sysToken") String sysToken,
                                                                   @RequestParam("havePlc") Integer havePlc) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return materialBatchService.findComboBoxList(userInfo, havePlc);
    }

    @PostMapping("/addIngredient")
    @ApiOperation(value = "新增配料")
    public ResponseResult addIngredient(@RequestHeader("sysToken") String sysToken, @RequestBody ProIngredientRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return ingredientService.addIngredient(rq, userInfo);
    }

    @PostMapping("/list/ingredients")
    @ApiOperation(value = "查询配料记录列表")
    public ResponseResult<List<ProIngredientDTO>> findIngredientList(@RequestHeader("sysToken") String sysToken,
                                                                     @RequestBody ProIngredientRQ rq, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return ingredientService.findList(rq, userInfo, pagination);
    }

    @GetMapping("/list/waitBlanking")
    @ApiOperation(value = "查询待下料列表")
    public ResponseResult<List<ProIngredientStatisticDTO>> findIngredientList(@RequestHeader("sysToken") String sysToken,
                                                                     @RequestParam("havePlc") Integer havePlc) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return ingredientService.findBlankingList(userInfo, havePlc);
    }

    @PostMapping("/blanking")
    @ApiOperation(value = "下料")
    public ResponseResult blanking(@RequestHeader("sysToken") String sysToken, @RequestBody ProBlankingRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return blankingService.blanking(rq, userInfo);
    }

    @PostMapping("/list/blankings")
    @ApiOperation(value = "查询下料记录列表")
    public ResponseResult<List<ProBlankingQueryDTO>>  findBlankingList(@RequestHeader("sysToken") String sysToken, @RequestBody ProBlankingRQ rq, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return blankingService.findList(rq, userInfo, pagination);
    }

    @PostMapping("/artificialFeed")
    @ApiOperation(value = "人工补料")
    public ResponseResult artificialFeed(@RequestHeader("sysToken") String sysToken, @RequestBody ProArtificialFeedRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return artificialFeedService.artificialFeed(rq, userInfo);
    }

    @PostMapping("/list/artificialFeeds")
    @ApiOperation(value = "查询人工补料记录列表")
    public ResponseResult<List<ProArtificialFeedDTO>> findArtificialFeedList(@RequestHeader("sysToken") String sysToken, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return artificialFeedService.findArtificialFeedList(userInfo, pagination);
    }

    @PostMapping("/findStorages")
    @ApiOperation(value = "根据产品id查询仓库列表")
    public ResponseResult<List<ProductStorageDTO>> findStorages(@RequestHeader("sysToken") String sysToken, Integer productId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return storageInventoryService.findStorages(productId);
    }

    @PostMapping("/addDeviceInspection")
    @ApiOperation(value = "新增设备巡检信息")
    public ResponseResult addDeviceInspection(@RequestHeader("sysToken") String sysToken, @RequestBody ProDeviceInspectionRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return deviceInspectionService.add(rq, userInfo);
    }

    @GetMapping("/list/deviceInspections")
    @ApiOperation(value = "查询巡检设备记录列表")
    public ResponseResult<DeviceInspectionDTO> findDeviceInspectionList(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return deviceInspectionService.findDeviceInspectionList(userInfo);
    }

    @PostMapping("/confirmDevice")
    @ApiOperation(value = "设备巡检确认结果")
    public ResponseResult confirmDeviceInspection(@RequestHeader("sysToken") String sysToken,
                                           @RequestBody ProDeviceInspectionConfirmRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return deviceInspectionService.confirmDeviceInspection(userInfo, rq);
    }

    @PostMapping("/addOreRecord")
    @ApiOperation(value = "新增矿热炉记录基本信息")
    public ResponseResult addOreRecord(@RequestHeader("sysToken") String sysToken, @RequestBody ProOreFurnaceRecordRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return oreFurnaceRecordService.addOreRecord(rq, userInfo);
    }

    @PostMapping("/getOreRecord/onduty")
    @ApiOperation(value = "查询矿热炉当前是否有记录员")
    public ResponseResult<ProOreRecordUserDTO> getOreRecordOnduty(@RequestHeader("sysToken") String sysToken, @RequestBody ProOreFurnaceRecordQueryRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return oreFurnaceRecordService.getOreRecordOnduty(rq, userInfo);
    }

    @PostMapping("/updateOreRecord")
    @ApiOperation(value = "更新矿热炉记录基本信息")
    public ResponseResult updateOreRecord(@RequestHeader("sysToken") String sysToken, @RequestBody ProOreFurnaceRecordRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return oreFurnaceRecordService.updateOreRecord(rq, userInfo);
    }

    @PostMapping("/addOreRecordDetail")
    @ApiOperation(value = "新增矿热炉记录明细信息")
    public ResponseResult addOreRecordDetail(@RequestHeader("sysToken") String sysToken, @RequestBody ProOreFurnaceRecordDetailRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return oreFurnaceRecordService.addOreRecordDetail(rq, userInfo);
    }

    @PostMapping("/updateOreRecordDetail")
    @ApiOperation(value = "更新矿热炉记录明细信息")
    public ResponseResult updateOreRecordDetail(@RequestHeader("sysToken") String sysToken, @RequestBody ProOreFurnaceRecordDetailRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return oreFurnaceRecordService.updateOreRecordDetail(rq, userInfo);
    }

    @GetMapping("/list/oreRecordDetail")
    @ApiOperation(value = "查询矿热炉记录明细信息")
    public ResponseResult<List<ProOreFurnaceRecordDetailDTO>> findOreRecordDetailList(@RequestHeader("sysToken") String sysToken,
                                                   @RequestParam("recordId") Long recordId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return oreFurnaceRecordService.findOreRecordDetailList(recordId, userInfo);
    }

    @PostMapping("/getOreRecordDetail")
    @ApiOperation(value = "根据矿热炉记录id查询矿热炉记录明细信息")
    public ResponseResult<ProOreFurnaceRecordDetailDTO>  getOreRecordDetail(@RequestHeader("sysToken") String sysToken,
                                                                            @RequestBody ProOreFurnaceRecordQueryRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return oreFurnaceRecordService.getOreRecordDetail(rq, userInfo);
    }

    @PostMapping("/noticeSample")
    @ApiOperation(value = "矿热炉通知取样")
    public ResponseResult addOreRecordDetail(@RequestHeader("sysToken") String sysToken, @RequestBody ProOreFurnaceSampleRQ rq) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return oreFurnaceSampleService.noticeSample(rq, userInfo);
    }

    @GetMapping("/handOver")
    @ApiOperation(value = "根据炉号id查询上次交班时的矿热炉记录明细信息")
    public ResponseResult<ProOreFurnaceRecordDetailDTO> getOreRecordHandOverDetail(@RequestHeader("sysToken") String sysToken, Integer furnaceId) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        if (ObjectUtils.isEmpty(furnaceId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.INTERFACE_PARAM_EMPTY);
        }
        return oreFurnaceRecordService.getOreRecordHandOverDetail(furnaceId, userInfo);
    }

    @GetMapping("/products")
    @ApiOperation(value = "获取库存中的产品列表")
    public ResponseResult<List<ProductStorageDTO>> findStorageProducts(@RequestHeader("sysToken") String sysToken) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return storageInventoryService.findStorageProducts(userInfo);
    }

    /**
     * @descriptin 条件查询矿热炉记录基本信息列表
     * @author xin.huang
     * @param sysToken
     * @param rq
     * @date 2019/11/5
     * @return
     */
    @PostMapping("/records")
    @ApiOperation(value = "条件查询矿热炉记录基本信息列表")
    public ResponseResult<List<ProOreFurnaceRecordDTO>> findOreRecords(@RequestHeader("sysToken") String sysToken,
                                                                       @RequestBody ProOreFurnaceRecordRQ rq, Page page) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo))  {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        Pagination pagination = PageUtils.transFromPage(page);
        return oreFurnaceRecordService.findOreRecords(rq, userInfo, pagination);
    }
}

