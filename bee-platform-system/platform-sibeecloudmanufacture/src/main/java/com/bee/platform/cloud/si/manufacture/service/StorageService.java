package com.bee.platform.cloud.si.manufacture.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.rq.SaleOutOfStockSearchRQ;
import com.bee.platform.cloud.si.manufacture.rq.BuyStorageRq;
import com.bee.platform.cloud.si.manufacture.rq.FinishedStorageRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * @ClassName: StorageService
 * @Description: 仓储Service
 * @Author: fei.sun
 * @Date: 2019/9/23 16:50
 * @Version: 1.0
 */
public interface StorageService {

    /**
     * 保存入库产品库存量
     * @param productStorageDTO
     * @param userInfo
     */
    void saveProduct(ProductStorageDTO productStorageDTO, AuthPlatformUserInfo userInfo);

    /**
     * 保存待入库产品明细
     * @param productPendingStorageDTO
     * @param userInfo
     */
    void savePendingStorageProduct(ProductPendingStorageDTO productPendingStorageDTO,AuthPlatformUserInfo userInfo);

    /**
     * 查询采购待入库
     * @param pagination
     * @param userInfo
     * @return
     */
    ResponseResult<List<BuyPendingStorageProductListDTO>> selectBuyPendingStorageInfo(Pagination pagination,AuthPlatformUserInfo userInfo);

    /**
     * 自由入库接口
     * @param freeStorageDetailDTO
     * @param userInfo
     */
    void saveFreeStorage(FreeStorageDetailDTO freeStorageDetailDTO, AuthPlatformUserInfo userInfo);

    /**
     * 保存待入库的产成品信息
     * @param finishedProductPendingStorageDTO
     * @param userInfo
     */
    void saveFinishedProductStorage(FinishedProductPendingStorageDTO finishedProductPendingStorageDTO, AuthPlatformUserInfo userInfo);

    /**
     * 查询所有待入库产成品
     * @param pagination
     * @param userInfo
     * @return
     */
    ResponseResult<List<FinishedProductPendingStorageDTO>> selectAllUnStorageFinishedProduct(Pagination pagination,AuthPlatformUserInfo userInfo);

    /**
     * 产成品批量入库
     * @param finishedProductStorageListDTO
     * @param userInfo
     */
    void saveFinishedProduct(FinishedProductStorageListDTO finishedProductStorageListDTO, AuthPlatformUserInfo userInfo);

    /**
     * 成品化验后，更新吨袋的规格并存入库存
     * @param proBagStorageSpecUpdateDTOS 规格
     * @param userInfo 用户信息
     */
    void updateFinishedProductSpecAndInsertStorage(List<ProBagStorageSpecUpdateDTO> proBagStorageSpecUpdateDTOS, AuthPlatformUserInfo userInfo);

    /**
     * 保存待出库产成品
     * @param finishedProductBeOutOfStorageDTO
     * @param userInfo
     */
    void saveFinishedProductOutOfStorage(FinishedProductBeOutOfStorageDTO finishedProductBeOutOfStorageDTO, AuthPlatformUserInfo userInfo);

    /**
     * 查询待装货的车辆信息
     * @return
     */
    ResponseResult<List<ContractCarDTO>> selectUnLoadContractCar(Pagination pagination,AuthPlatformUserInfo userInfo);


    /**
     * web查询销售待出库列表
     * @param rq 请求参数
     * @param userInfo 用户信息
     * @return 销售待出库列表
     */
    ResponseResult<List<ContractCarDTO>> webSearchSaleToBeOutOfStockList(SaleOutOfStockSearchRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 根据吨袋编号查询吨袋信息
     * @param tonBagNumber
     * @return
     */
    TonBagDTO selectTonBagInfo(String tonBagNumber,AuthPlatformUserInfo userInfo);

    /**
     * 合同详情确认入库
     * @param machineIds
     * @param userInfo
     */
    void bulkConfirmProduct(List<ConfirmProductDTO> machineIds, AuthPlatformUserInfo userInfo);

    /**
     * 合同详情折价入库
     * @param bulkPendingStorageProductDTO
     * @param userInfo
     */
    void bulkDiscountedProduct(BulkPendingStorageProductDTO bulkPendingStorageProductDTO, AuthPlatformUserInfo userInfo);

    /**
     * 销售吨袋批量出库
     * @param finishedProductOutListDTO
     * @param userInfo
     */
    void bulkOutTonBag(FinishedProductOutListDTO finishedProductOutListDTO, AuthPlatformUserInfo userInfo);

    /**
     * 查询仓库中所有的产品种类
     * @param storageId
     * @return
     */
    List<ProductDTO> selectProductByStorageId(String storageId);

    /**
     * 领用入库
     * @param pickOutProductDTO
     * @param userInfo
     */
    void pickOutProduct(PickOutProductDTO pickOutProductDTO, AuthPlatformUserInfo userInfo);

    /**
     * 生产出库
     * @param productionOutStorageDetailDTO
     * @param userInfo
     */
    void saveProductionOutStorage(ProductionOutStorageDetailDTO productionOutStorageDetailDTO, AuthPlatformUserInfo userInfo);

    /**
     * 根据产品类别查询库存总量
     * @param productType
     * @param productName
     * @return
     */
    ResponseResult<List<StorageManagerListDTO>> selectStorageManagerList(Integer productType,String productName,Pagination pagination
            ,AuthPlatformUserInfo userInfo);

    /**
     * 盘库
     * @param revisionStorageDTO
     * @param userInfo
     */
    void revisionStorage(RevisionStorageDTO revisionStorageDTO, AuthPlatformUserInfo userInfo);

    /**
     * 查询盘库的记录
     * @param storageId
     * @param productId
     * @returna
     */
    List<RevisionStorageRecordDTO> selectRevisionStorageRecords(String storageId, String productId);

    /**
     * 查询一键平库列表
     * @param userInfo a
     * @return b
     */
    List<BalanceStorageDTO> selectBalanceStorages(AuthPlatformUserInfo userInfo);

    /**
     * 一键平库
     * @param userInfo a
     */
    void balanceStorages(AuthPlatformUserInfo userInfo);

    /**
     *  查询库存管理出入库记录
     * @param storageId
     * @param productId
     * @return
     */
    StorageManagerRecordListDTO selectInAndOutStorageRecords(Integer storageId, Integer productId,Integer productSpecId);

    /**
     * 查询入库记录
     * @return
     */
    InStorageRecordListDTO getInStorageDetail(AuthPlatformUserInfo userInfo);


    /**
     * 查询出库记录
     * @param userInfo
     * @return
     */
    OutStorageRecordListDTO getOutStorageRecordDetail(AuthPlatformUserInfo userInfo);

    /**
     * 查询产成品入库详情
     * @param furnaceNumber
     * @param furnaceTimes
     * @param scheduling
     * @param storageId
     * @param storageTime
     * @return
     */
    FinishedProductInStorageDetailDTO getFinishedProductInStorageDetail(String furnaceNumber, String furnaceTimes
            , String scheduling, String storageId,String storageTime);

    /**
     * 查看销售出库记录详情
     * @param contractId 合同号
     * @param storageId 仓库id
     * @return 实体
     */
    List<SaleOutStorageDetailDTO> getSaleOutStorageRecordDetail(String contractId,Integer storageId);

    /**
     * 查询采购入库详情
     * @param contractId
     * @param storageId
     * @return
     */
    List<BuyInStorageDetailDTO> getBuyInStorageDetail(String contractId, Integer storageId);

    /**
     * 入库扫描吨袋信息
     * @param tonBagNumber
     * @return
     */
    TonBagDTO selectInStorageTonBagInfo(String tonBagNumber,AuthPlatformUserInfo userInfo);

    /**
     * 销售出库产品规格下拉框
     * @param userInfo a
     * @param productId b
     * @return v
     */
    List<StorageProductSpecDTO> selectSaleProductSpec(AuthPlatformUserInfo userInfo,Integer productId);

    /**
     * 根据合同编号模糊查询待入库信息
     * @param pagination
     * @param userInfo
     * @param contractNum
     * @return
     */
	ResponseResult<List<BuyPendingStorageListDTO>> selectBuyPendingStorageInfoByConNum(Pagination pagination,
			AuthPlatformUserInfo userInfo, String contractNum);

    /**
     * web 条件搜索销售已出库列表
     * @param rq 请求参数
     * @param userInfo 用户信息
     * @return 销售已出库列表
     */
    ResponseResult<List<SaleOutOfStockDTO>> searchSaleOutOfStockList(SaleOutOfStockSearchRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * web 根据id查询销售待出库详情
     * @param id id
     * @param userInfo 用户信息
     * @return 销售待出库详情
     */
    ResponseResult<ContractCarDTO> getSaleToBeDeliveredById(Integer id, AuthPlatformUserInfo userInfo);

    /**
     * web 根据id查询销售已出库详情
     * @param id id
     * @param userInfo 用户信息
     * @return 销售已出库详情
     */
    SaleOutOfStockDetailDTO getSaleOutOfStockById(Integer id, AuthPlatformUserInfo userInfo);

	/**
	 * 根据条件查询待入库信息
	 * @param pagination
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	ResponseResult<List<BuyPendingStorageListDTO>> selectBuyPendingStorageInfoByConditional(Pagination pagination,
			AuthPlatformUserInfo userInfo, BuyStorageRq rq);
	/**
	 * 根据条件查询已入库信息
	 * @param pagination
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	ResponseResult<List<BuyStorageListDTO>> selectBuyStorageInfoByConditional(Pagination pagination,
			AuthPlatformUserInfo userInfo, BuyStorageRq rq);

	/**
	 * 根据buyProductPendingStorageId查询采购入库详情信息
	 * @param userInfo
	 * @param buyProductPendingStorageId
	 * @return
	 */
	ResponseResult<BuyPendingStorageProductMsgDTO> selectBuyStorageMsg(AuthPlatformUserInfo userInfo,
			String buyProductPendingStorageId);

	/**
	 * 查询自由入库详情
	 * @param userInfo
	 * @return
	 */
	ResponseResult<List<FreeInStorageRecordDTO>> selectFreeInStorageRecord(AuthPlatformUserInfo userInfo,Pagination pagination);

	/**
	 * 根据条件查询产成品未/已入库信息
	 * @param pagination
	 * @param userInfo
	 * @return
	 */
	ResponseResult<List<FinishedProductPendingStorageListDTO>> selectFinishedStorageByConditional(Pagination pagination,
			AuthPlatformUserInfo userInfo,FinishedStorageRq rq);
    /**
     * web 查询新增销售出库详情
     * @param userInfo 用户信息
     * @return 新增销售出库详情
     */
    List<PickOutStorageDetailDTO> getSaleNewOutOfStockDetails(AuthPlatformUserInfo userInfo);

	/**
	 * @Description 获取仓库树
	 * @author chenxm66777123
	 * @Date 2019/11/27 10:04
	 * @version 1.0.0
	 */
    ResponseResult<List<StorageTreeDTO>> getStorageTree(AuthPlatformUserInfo userInfo);

    /**
     * @Description 根据仓库id获取现存量
     * @author chenxm66777123
     * @Date 2019/11/27 11:00
     * @version 1.0.0
     */
    ResponseResult<List<StockDTO>> getStockByStorageId(List<Integer> storageId,AuthPlatformUserInfo userInfo,Page page);

}
