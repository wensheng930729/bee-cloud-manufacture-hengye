package com.bee.platform.cloud.si.manufacture.controller;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.ConfigRepository;
import com.bee.platform.cloud.si.manufacture.rq.SaleOutOfStockSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.BuyStorageRq;
import com.bee.platform.cloud.si.manufacture.rq.FinishedStorageRq;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.factoryconfig.ConfigRepositoryService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import io.swagger.models.auth.In;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import javax.validation.Valid;

/**
 * @ClassName: StorageController
 * @Description: 仓储Controller
 * @Author: fei.sun
 * @Date: 2019/9/23 13:53
 * @Version: 1.0
 */
@RestController
@RequestMapping(value = "/storage")
@Slf4j
@CrossOrigin(origins = "*")
@Api(value = "仓储相关接口", tags = "仓储相关接口")
public class StorageController {

    private final StorageService storageService;

    private final UserInfoUtils userInfoUtils;

    @Autowired
    private ConfigRepositoryService configRepositoryService;

    @Autowired
    public StorageController(StorageService storageService,UserInfoUtils userInfoUtils){
        this.storageService = storageService;
        this.userInfoUtils = userInfoUtils;
    }

    @ApiOperation("合同详情确认入库接口")
    @PostMapping("/bulkConfirmProduct")
    public ResponseResult bulkConfirmProduct(@RequestHeader("sysToken") String sysToken
            ,@RequestBody List<ConfirmProductDTO> machineIds){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.bulkConfirmProduct(machineIds,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation("合同详情折价入库接口")
    @PostMapping("/bulkDiscountedProduct")
    public ResponseResult bulkDiscountedProduct(@RequestHeader("sysToken") String sysToken
            ,@RequestBody BulkPendingStorageProductDTO bulkPendingStorageProductDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.bulkDiscountedProduct(bulkPendingStorageProductDTO,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation(value = "采购待入库页面查询")
    @GetMapping("/selectBuyPendingStorageInfo")
    public ResponseResult<List<BuyPendingStorageProductListDTO>> selectBuyPendingStorageInfo(@RequestHeader("sysToken") String sysToken,Page page){
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        return storageService.selectBuyPendingStorageInfo(pagination,userInfo);
    }

    @ApiOperation(value = "采购入库接口")
    @PostMapping("/saveProduct")
    public ResponseResult saveProduct(@RequestHeader("sysToken") String sysToken,@RequestBody ProductStorageDTO productStorageDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.saveProduct(productStorageDTO,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation("自由入库接口")
    @PostMapping("/saveFreeStorage")
    public ResponseResult saveFreeStorage(@RequestHeader("sysToken") String sysToken,@RequestBody FreeStorageDetailDTO freeStorageDetailDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.saveFreeStorage(freeStorageDetailDTO,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @ApiOperation("添加产成品待入库数据（不需调用，后端成品状态时调用）")
    @PostMapping("/saveFinishedProductStorage")
    public ResponseResult saveFinishedProductStorage(@RequestHeader("sysToken") String sysToken
            ,@RequestBody FinishedProductPendingStorageDTO finishedProductPendingStorageDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.saveFinishedProductStorage(finishedProductPendingStorageDTO,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @ApiOperation("查询所有待入库产成品")
    @GetMapping("/selectAllUnStorageFinishedProduct")
    public ResponseResult<List<FinishedProductPendingStorageDTO>> selectAllUnStorageFinishedProduct(@RequestHeader("sysToken") String sysToken
            ,Page page){
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        return storageService.selectAllUnStorageFinishedProduct(pagination,userInfo);
    }


    @ApiOperation("产成品批量入库")
    @PostMapping("/saveFinishedProduct")
    public ResponseResult saveFinishedProduct(@RequestHeader("sysToken") String sysToken
            ,@RequestBody FinishedProductStorageListDTO finishedProductStorageListDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.saveFinishedProduct(finishedProductStorageListDTO,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @ApiOperation("查询待装货的车辆信息")
    @GetMapping("/selectUnLoadContractCar")
    public ResponseResult<List<ContractCarDTO>> selectUnLoadContractCar(@RequestHeader("sysToken") String sysToken,Page page){
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        return storageService.selectUnLoadContractCar(pagination,userInfo);
    }


    /******************web 销售出库 相关接口**********************/
    //  -----web 销售待出库-----------

    @ApiOperation("---- web--销售---搜索待出库列表")
    @PostMapping("/webSearchSaleToBeOutOfStockList")
    public ResponseResult<List<ContractCarDTO>> webSearchSaleToBeOutOfStockList(@RequestHeader("sysToken") String sysToken, @RequestBody SaleOutOfStockSearchRQ rq){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        return storageService.webSearchSaleToBeOutOfStockList(rq,userInfo);
    }

    // ----web 销售待出库根据id查询详情

    @ApiOperation("根据id查询待销售待出库详情---- web---销售---待出库详情")
    @GetMapping("/getSaleToBeDeliveredById/{id}")
    public ResponseResult<ContractCarDTO> getSaleToBeDeliveredById(@RequestHeader("sysToken") String sysToken,@PathVariable("id") Integer id){

        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        return storageService.getSaleToBeDeliveredById(id,userInfo);
    }



    // -------web 销售已出库列表-----------------------

    @ApiOperation("web查询销售已出库列表-----web--销售--已出库列表")
    @PostMapping("/searchSaleOutOfStockList")
    public ResponseResult<List<SaleOutOfStockDTO>> searchSaleOutOfStockList(@RequestHeader("sysToken") String sysToken, @RequestBody SaleOutOfStockSearchRQ rq){

        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        return storageService.searchSaleOutOfStockList(rq,userInfo);
    }


    // ----web 销售已出库根据id查询详情

    @ApiOperation("根据id查询销售已出库详情---- web--销售--已出库详情")
    @GetMapping("/getSaleOutOfStockById/{id}")
    public ResponseResult<SaleOutOfStockDetailDTO> getSaleOutOfStockById(@RequestHeader("sysToken") String sysToken,@PathVariable("id") Integer id){

        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        SaleOutOfStockDetailDTO dto= storageService.getSaleOutOfStockById(id,userInfo);

        return  ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }


    // ----web 查看销售新增出库详情

    @ApiOperation("查询销售新增出库详情---- web查看--销售----新增出库详情")
    @GetMapping("/getSaleNewOutOfStockDetails")
    public ResponseResult<List<PickOutStorageDetailDTO>> getSaleNewOutOfStockDetails(@RequestHeader("sysToken") String sysToken){

        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);

        List<PickOutStorageDetailDTO> dto= storageService.getSaleNewOutOfStockDetails(userInfo);

        return  ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }

    /******************web 销售出库 相关接口 end**********************/

    @ApiOperation("出库扫描返回一条吨袋信息")
    @GetMapping("/selectTonBagInfo")
    public ResponseResult<TonBagDTO> selectTonBagInfo(@RequestParam(value = "tonBagNumber") String tonBagNumber
            ,@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,storageService.selectTonBagInfo(tonBagNumber,userInfo));
    }


    @ApiOperation("入库扫描返回一条吨袋信息")
    @GetMapping("/selectInStorageTonBagInfo")
    public ResponseResult<TonBagDTO> selectInStorageTonBagInfo(@RequestParam(value = "tonBagNumber") String tonBagNumber
            ,@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,storageService.selectInStorageTonBagInfo(tonBagNumber,userInfo));
    }

    // TODO -----提交出库信息-----------

    @ApiOperation("销售成品出库")
    @PostMapping("/bulkOutTonBag")
    public ResponseResult bulkOutTonBag(@RequestHeader("sysToken") String sysToken,@RequestBody FinishedProductOutListDTO finishedProductOutListDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.bulkOutTonBag(finishedProductOutListDTO,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @ApiOperation("仓库下的领用物下拉框")
    @GetMapping("/selectProductByStorage")
    public ResponseResult<List<ProductDTO>> selectProductByStorageId(@RequestParam("storageId") String storageId){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,storageService.selectProductByStorageId(storageId));
    }

    //  TODO ----新增出库-----


    @ApiOperation("新增领用出库")
    @PostMapping("/pickOutProduct")
    public ResponseResult pickOutProduct(@RequestHeader("sysToken") String sysToken,@RequestBody PickOutProductDTO pickOutProductDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.pickOutProduct(pickOutProductDTO,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @ApiOperation("新增生产出库（下料时调用，不用前端调用）")
    @PostMapping("/saveProductionOutStorage")
    public ResponseResult saveProductionOutStorage(@RequestHeader("sysToken") String sysToken
            ,@RequestBody ProductionOutStorageDetailDTO productionOutStorageDetailDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.saveProductionOutStorage(productionOutStorageDetailDTO,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }



    @ApiOperation("查询入库记录")
    @GetMapping("/getInStorageRecord")
    public ResponseResult<InStorageRecordListDTO> getInStorageRecord(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        InStorageRecordListDTO inStorageRecordListDTO = storageService.getInStorageDetail(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,inStorageRecordListDTO);
    }


    @ApiOperation("查询产成品入库详情")
    @GetMapping("/getFinishedProductInStorageDetail")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "furnaceNumber",value = "炉号",required = true),
            @ApiImplicitParam(name = "furnaceTimes",value = "炉次",required = true),
            @ApiImplicitParam(name = "scheduling",value = "班次",required = true),
            @ApiImplicitParam(name = "storageId",value = "仓库id",required = true),
            @ApiImplicitParam(name = "storageTime",value = "入库时间，格式为 yyyy-MM-dd",required = true)
    })
    public ResponseResult<FinishedProductInStorageDetailDTO> getFinishedProductInStorageDetail(String furnaceNumber,String furnaceTimes
            ,String scheduling,String storageId,String storageTime){
        FinishedProductInStorageDetailDTO finishedProductInStorageDetail =
                storageService.getFinishedProductInStorageDetail(furnaceNumber,furnaceTimes
                ,scheduling,storageId,storageTime);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,finishedProductInStorageDetail);
    }


    @ApiOperation("查询采购入库详情")
    @GetMapping("/getBuyInStorageDetail")
    public ResponseResult<List<BuyInStorageDetailDTO>> getBuyInStorageDetail(String contractId,Integer storageId){
        List<BuyInStorageDetailDTO> buyInStorageDetailDTOS = storageService.getBuyInStorageDetail(contractId,storageId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,buyInStorageDetailDTOS);
    }


    // TODO ------已出库列表-----------


    @ApiOperation("查询出库记录")
    @GetMapping("/getOutStorageRecordDetail")
    public ResponseResult<OutStorageRecordListDTO> getOutStorageRecord(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        OutStorageRecordListDTO outStorageRecordListDTO = storageService.getOutStorageRecordDetail(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,outStorageRecordListDTO);
    }

    // TODO ------已出库详情-----------


    @ApiOperation("查看销售出库记录详情")
    @GetMapping("/getSaleOutStorageRecordDetail")
    public ResponseResult<List<SaleOutStorageDetailDTO>> getSaleOutStorageRecordDetail(@RequestParam("contractId") String contractId
            ,@RequestParam("storageId") Integer storageId){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,storageService.getSaleOutStorageRecordDetail(contractId,storageId));
    }

    /******************库存管理相关接口**********************/

    @ApiOperation("查询成品、主料、辅料以及其他的列表接口")
    @GetMapping("/selectStorageManagerList")
    public ResponseResult<List<StorageManagerListDTO>> selectStorageManagerList(@RequestHeader("sysToken") String sysToken
            ,@RequestParam("productType") Integer productType
            ,@RequestParam(value = "productName",required = false) String productName,Page page){
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        return storageService.selectStorageManagerList(productType,productName,pagination,userInfo);
    }


    @ApiOperation("盘库接口")
    @PostMapping("/revisionStorage")
    public ResponseResult revisionStorage(@RequestHeader("sysToken") String sysToken,@RequestBody RevisionStorageDTO revisionStorageDTO){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.revisionStorage(revisionStorageDTO,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    @ApiOperation("盘库记录接口")
    @GetMapping("/selectRevisionStorageRecords")
    public ResponseResult<List<RevisionStorageRecordDTO>> selectRevisionStorageRecords(@RequestParam("storageId") String storageId
            ,@RequestParam("productId") String productId){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,storageService.selectRevisionStorageRecords(storageId,productId));
    }


    @ApiOperation("库存管理出/入库记录接口")
    @GetMapping("/selectInAndOutStorageRecords")
    public ResponseResult<StorageManagerRecordListDTO> selectInAndOutStorageRecords(@RequestParam("storageId") Integer storageId
            ,@RequestParam("productId") Integer productId,@RequestParam("productSpecId") Integer productSpecId){
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,storageService.selectInAndOutStorageRecords(storageId,productId,productSpecId));
    }


    @ApiOperation("一键平库查询接口")
    @GetMapping("/selectBalanceStorages")
    public ResponseResult<List<BalanceStorageDTO>> selectBalanceStorages(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,storageService.selectBalanceStorages(userInfo));
    }

    @ApiOperation("一键平库")
    @GetMapping("/balanceStorages")
    public ResponseResult balanceStorages(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        storageService.balanceStorages(userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @ApiOperation("查询销售出库的规格")
    @GetMapping("/selectSaleProductSpec")
    public ResponseResult<List<StorageProductSpecDTO>> selectSaleProductSpec(@RequestHeader("sysToken") String sysToken,Integer productId){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        List<StorageProductSpecDTO> storageProductSpecDTOS = storageService.selectSaleProductSpec(userInfo,productId);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,storageProductSpecDTOS);
    }


    /***********************web端采购入库接口******************************/
    @ApiOperation(value = "web-采购待入库页面查询--根据条件查询查询")
    @GetMapping("/selectBuyPendingStorageInfoByConditional")
    public ResponseResult<List<BuyPendingStorageListDTO>> selectBuyPendingStorageInfoByConditional(@RequestHeader("sysToken") String sysToken,
    		 BuyStorageRq rq,Page page){
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return storageService.selectBuyPendingStorageInfoByConditional(pagination,userInfo,rq);
    }

    @ApiOperation(value = "web-采购已入库页面查询--根据条件查询查询")
    @GetMapping("/selectBuyStorageInfoByConditional")
    public ResponseResult<List<BuyStorageListDTO>> selectBuyStorageInfoByConditional(@RequestHeader("sysToken") String sysToken,
    		 BuyStorageRq rq,Page page){
        Pagination pagination = PageUtils.transFromPage(page);
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return storageService.selectBuyStorageInfoByConditional(pagination,userInfo,rq);
    }

    @ApiOperation(value = "web-采购待/已入库页面--查看详情--根据buyProductPendingStorageId查询详情")
    @GetMapping("/selectBuyStorageMsg")
    public ResponseResult<BuyPendingStorageProductMsgDTO> selectBuyStorageMsg(@RequestHeader("sysToken") String sysToken,
    		@RequestParam("buyProductPendingStorageId") String buyProductPendingStorageId){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return storageService.selectBuyStorageMsg(userInfo,buyProductPendingStorageId);
    }

    @ApiOperation(value = "web-采购已入库页面--查看新增自由入库详情")
    @GetMapping("/selectFreeInStorageRecord")
    public ResponseResult<List<FreeInStorageRecordDTO>> selectFreeInStorageRecord(@RequestHeader("sysToken") String sysToken,Page page){
    	Pagination pagination = PageUtils.transFromPage(page);
    	AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return storageService.selectFreeInStorageRecord(userInfo,pagination);
    }


    /***********************web端产成品入库接口******************************/


    @ApiOperation("web-查询所有待/已入库产成品")
    @GetMapping("/selectFinishedStorageByConditional")
    public ResponseResult<List<FinishedProductPendingStorageListDTO>> selectFinishedStorageByConditional(@RequestHeader("sysToken") String sysToken,
  		@Valid  FinishedStorageRq rq,Page page){
      Pagination pagination = PageUtils.transFromPage(page);
      AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
      if (ObjectUtils.isEmpty(userInfo)) {
          return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
      }
      return storageService.selectFinishedStorageByConditional(pagination,userInfo,rq);
  }




    


    /***********************web端现存量查询接口******************************/
    @ApiOperation(value = "获取仓库树",notes = "获取仓库树")
    @GetMapping("/getStorageTree")
    public ResponseResult<List<StorageTreeDTO>> getStorageTree(@RequestHeader("sysToken") String sysToken){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return storageService.getStorageTree(userInfo);
    }

    @ApiOperation(value = "根据仓库id获取现存量",notes = "根据仓库id获取现存量")
    @ApiImplicitParam(name = "storageId", value = "仓库id,不传则默认查询全部")
    @GetMapping("/getStockByStorageId")
    public ResponseResult<List<StockDTO>> getStockByStorageId(@RequestHeader("sysToken") String sysToken, Integer storageId,Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<Integer> storageIds = new ArrayList<>();
        storageIds.add(storageId);
        return storageService.getStockByStorageId(storageIds,userInfo,page);
    }


    @ApiOperation(value = "根据仓库类型获取现存量",notes = "根据仓库类型获取现存量")
    @ApiImplicitParam(name = "storageType", value = "仓库类型")
    @GetMapping("/getStockByStorageType")
    public ResponseResult<List<StockDTO>> getStockByStorageType(@RequestHeader("sysToken") String sysToken, Integer storageType,Page page){
        AuthPlatformUserInfo userInfo = userInfoUtils.getUserInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        List<ConfigRepository> configRepositories = configRepositoryService.selectList(
                new EntityWrapper<>(new ConfigRepository().setEnterpriseId(userInfo.getOrgId())
                        .setType(storageType)
                        .setStatus(Status.TRUE.getKey())
                        .setDeleted(Status.FALSE.getKey())));
        if(CollectionUtils.isEmpty(configRepositories)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>());
        }
        List<Integer> storageIds =  configRepositories.stream().map(e-> e.getId()).collect(Collectors.toList());
        return storageService.getStockByStorageId(storageIds,userInfo,page);
    }

}