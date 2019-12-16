package com.bee.platform.cloud.si.manufacture.controller;


import com.bee.platform.cloud.si.manufacture.dto.InventoryCategoryDTO;
import com.bee.platform.cloud.si.manufacture.dto.InventoryOrderDTO;
import com.bee.platform.cloud.si.manufacture.dto.InventoryTypeDTO;
import com.bee.platform.cloud.si.manufacture.dto.StockInventoryListDTO;
import com.bee.platform.cloud.si.manufacture.rq.InventoryRQ;
import com.bee.platform.cloud.si.manufacture.rq.InventorySearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.StockInventoryRQ;
import com.bee.platform.cloud.si.manufacture.service.StockInventoryService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Path;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 库存盘点主表
 * </p>
 *
 * @author junyang.li123
 * @since 2019-11-25
 */
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/stockInventory")
@Api(value = "stockInventory",tags = "web 端库存盘点相关接口")
public class StockInventoryController {

    @Autowired
    private StockInventoryService stockInventoryService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @GetMapping("/inventory/type")
    @ApiOperation(value = "盘点单类型查询")
    public ResponseResult<List<InventoryTypeDTO>> getInventoryType(){
        return stockInventoryService.getInventoryType();
    }

    @GetMapping("/inventoryType/desc")
    @ApiOperation(value = "根据盘点分类获得下拉列表详细")
    @ApiImplicitParam(value = "类型code",name = "inventoryTypeCode",required = true)
    public ResponseResult<List<InventoryCategoryDTO>> getInventoryTypeDesc(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                                           Integer inventoryTypeCode){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        return stockInventoryService.getInventoryTypeDesc(userInfo,inventoryTypeCode);
    }

    @PostMapping("/create/inventoryOrder")
    @ApiOperation(value = "创建库存盘点单，并返回待盘点数据")
    public ResponseResult<InventoryOrderDTO> createInventoryOrder(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                                  @RequestBody @Valid InventoryRQ rq){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        return stockInventoryService.createInventoryOrder(userInfo,rq);
    }

    @PostMapping("/save/inventoryInfo")
    @ApiOperation(value = "保存盘点单")
    public ResponseResult<ResCodeEnum> saveInventoryInfo(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                         @RequestBody @Valid StockInventoryRQ rq){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        return stockInventoryService.saveInventoryInfo(userInfo,rq);
    }

    @GetMapping("/inventoryInfo/list")
    @ApiOperation(value = "盘点单列表查询")
    public ResponseResult<List<StockInventoryListDTO>> getInventoryInfoList(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                                            InventorySearchRQ rq){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        return stockInventoryService.getInventoryInfoList(userInfo,rq);
    }

    @GetMapping("/inventoryInfo/{inventoryOrderId}")
    @ApiOperation(value = "根据盘点单号查询盘点单详细")
    @ApiImplicitParam(value = "盘点单编号",name = "inventoryOrderId",dataType = "path")
    public ResponseResult<InventoryOrderDTO> getInventoryInfoById(@RequestHeader(ConstantsUtil.SYS_TOKEN)String sysToken,
                                                                            @PathVariable("inventoryOrderId")String inventoryOrderId){
        AuthPlatformUserInfo userInfo=userInfoUtils.getUserInfo(sysToken);
        return stockInventoryService.getInventoryInfoById(userInfo,inventoryOrderId);
    }
}

