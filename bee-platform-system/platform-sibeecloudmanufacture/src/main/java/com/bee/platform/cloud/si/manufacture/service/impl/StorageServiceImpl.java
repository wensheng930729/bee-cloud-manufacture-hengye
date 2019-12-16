package com.bee.platform.cloud.si.manufacture.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumConfig;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.enums.EnumStorage;
import com.bee.platform.cloud.si.manufacture.rq.BuyStorageRq;
import com.bee.platform.cloud.si.manufacture.rq.FinishedStorageRq;
import com.bee.platform.cloud.si.manufacture.rq.SaleOutOfStockSearchRQ;
import com.bee.platform.cloud.si.manufacture.service.RevisionStorageDetailService;
import com.bee.platform.cloud.si.manufacture.service.StorageInventoryService;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyWeightMachineService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.LocalDateUtils;
import com.bee.platform.common.utils.PageUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @ClassName: StorageServiceImpl
 * @Description: 仓储管理Service
 * @Author: fei.sun
 * @Date: 2019/9/23 16:50
 * @Version: 1.0
 */
@Slf4j
@Service
public class StorageServiceImpl implements StorageService {

    @Autowired
    private StorageInventoryMapper storageInventoryMapper;
    @Autowired
    private BuyGoodsPendingStorageMapper buyGoodsPendingStorageMapper;
    @Autowired
    private FinishedProductPendingStorageMapper finishedProductPendingStorageMapper;
    @Autowired
    private FinishedProductBeOutOfStorageMapper finishedProductBeOutOfStorageMapper;
    @Autowired
    private FinishedProductOutStorageDetailMapper finishedProductOutStorageDetailMapper;
    @Autowired
    private BuyWeightMachineService buyWeightMachineService;
    @Autowired
    private BuyWeightMachineMapper buyWeightMachineMapper;
    @Autowired
    private RevisionStorageDetailMapper revisionStorageDetailMapper;
    @Autowired
    private ConfigProductMapper configProductMapper;
    @Autowired
    private ConfigRepositoryMapper configRepositoryMapper;
    @Autowired
    private ProductionOutStorageDetailMapper productionOutStorageDetailMapper;
    @Autowired
    private FreeStorageDetailMapper freeStorageDetailMapper;
    @Autowired
    private PickOutStorageDetailMapper pickOutStorageDetailMapper;
    @Autowired
    private InAndOutStorageRecordMapper inAndOutStorageRecordMapper;
    @Autowired
    private SampleAssayResultMapper sampleAssayResultMapper;
    @Autowired
    private BuySampleMapper buySampleMapper;
    @Autowired
    private StorageInventoryService storageInventoryService;
    @Autowired
    private RevisionStorageDetailService revisionStorageDetailService;
    @Autowired
    private FinishedProductFreeOutMapper finishedProductFreeOutMapper;
    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;
    @Autowired
    private ProBaggingMapper proBaggingMapper;

    private static final String GET_CODE_METHOD = "getKey";

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveProduct(ProductStorageDTO productStorageDTO, AuthPlatformUserInfo userInfo) {
        if(ObjectUtils.isEmpty(productStorageDTO)||StringUtils.isBlank(productStorageDTO.getBuyProductPendingStorageId())){
            throw new BusinessException(ResCodeEnum.MISS_NECESSARY_PARAM, ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        //入库时间
        String buyProductPendingStorageId = productStorageDTO.getBuyProductPendingStorageId();
        BuyGoodsPendingStorage buyGoodsPendingStorage = buyGoodsPendingStorageMapper.selectOne(new BuyGoodsPendingStorage()
                .setBuyProductPendingStorageId(buyProductPendingStorageId).setStatus(EnumStorage.Status.normal.getKey()));
        log.info("根据待入库业务id: {}, 查询到的数据是 : {}",buyProductPendingStorageId,buyGoodsPendingStorage);
        if(ObjectUtils.isEmpty(buyGoodsPendingStorage)){
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_DATA);
        }
        if (EnumStorage.PutStorage.storage.getKey().equals(buyGoodsPendingStorage.getPutStorage())) {
            log.info("该车辆已经做过入库操作，不能重复提交，{}",productStorageDTO);
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.STORAGE_REPEATED_SUBMISSION);
        }
        buyGoodsPendingStorage.setModifier(userInfo.getName()).setModifyId(userInfo.getId()).setModifyTime(LocalDateTime.now());
        buyGoodsPendingStorage.setPutStorage(EnumStorage.PutStorage.storage.getKey()).setStorageTime(LocalDateTime.now())
                .setStorageId(productStorageDTO.getStorageId()).setStorageName(productStorageDTO.getStorageName())
                .setActualProductNumber(productStorageDTO.getProductNumber())
                .updateById();
        Integer productId = productStorageDTO.getProductId();
        //产品规格
        Integer productSpecId = buyGoodsPendingStorage.getProductSpecId();
        String productSpecName = buyGoodsPendingStorage.getProductSpecName();
        StorageInventory storageInventory = storageInventoryMapper.selectOne(new StorageInventory().setProductId(productId)
                .setStorageId(productStorageDTO.getStorageId()).setProductSpecId(productSpecId)
                .setStatus(EnumStorage.Status.normal.getKey()));
        if(ObjectUtils.isEmpty(storageInventory)){
            //新增一条数据,后面需要优化
            storageInventory = new StorageInventory();
            BeanUtils.copyProperties(productStorageDTO,storageInventory);
            storageInventory.setStatus(EnumStorage.Status.normal.getKey())
                    .setProductSpecId(productSpecId).setProductSpecName(productSpecName)
                    .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                    .setCreateId(userInfo.getId()).setCreateTime(LocalDateTime.now()).setCreator(userInfo.getName());
            storageInventory.insert();
        }else {
            //更新总量,后面在优化
            BigDecimal productNumber = productStorageDTO.getProductNumber();
            storageInventory.setProductNumber(storageInventory.getProductNumber().add(productNumber))
                    .setModifier(userInfo.getName()).setModifyId(userInfo.getId()).setModifyTime(LocalDateTime.now());
            storageInventory.updateById();
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bulkConfirmProduct(List<ConfirmProductDTO> confirmProductDTOS, AuthPlatformUserInfo userInfo) {
        List<String> machineIds = new ArrayList<>();
        if(!CollectionUtils.isEmpty(confirmProductDTOS)){
            machineIds = confirmProductDTOS.stream().map(ConfirmProductDTO::getMachineId).collect(Collectors.toList());
        }
        //通过磅单号获取需要的数据
        List<ProductPendingStorageDTO> productPendingStorageDTOS = transferToPendingStorageDTO(machineIds,EnumStorage.ProcessMode.confirm_storage.getKey());
        if(!CollectionUtils.isEmpty(productPendingStorageDTOS)){
            productPendingStorageDTOS.forEach(productPendingStorageDTO -> savePendingStorageProduct(productPendingStorageDTO,userInfo));
        }
    }

    private List<ProductPendingStorageDTO> transferToPendingStorageDTO(List<String> machineIds,Integer processMode) {
        if(CollectionUtils.isEmpty(machineIds)){
            return null;
        }
        List<ProductPendingStorageDTO> productPendingStorageDTOS = new ArrayList<>();
        //根据磅单号查询地磅数据信息
        List<BuyWeightMachine> buyWeightMachines = buyWeightMachineMapper.selectList(new EntityWrapper<BuyWeightMachine>()
                .in("machine_id", machineIds).eq("status", EnumStorage.Status.normal.getKey()));
        //组装需要的数据
        if(!CollectionUtils.isEmpty(buyWeightMachines)){
            buyWeightMachines.forEach(buyWeightMachine -> {

                //不合格车辆列表入库确认时，更新榜单中的是否确认信息
                buyWeightMachine.setInStorageConfirm(EnumSampleRelation.InStorageConfirmType.YES.getKey());
                // 添加其处理方式
                buyWeightMachine.setHandleType(processMode);
                buyWeightMachineMapper.updateById(buyWeightMachine);

                ProductPendingStorageDTO productPendingStorageDTO = new ProductPendingStorageDTO();
                //产品id
                Integer productId = buyWeightMachine.getProductId();
                ConfigProduct configProduct = configProductMapper.selectById(productId);
                //查询产品的规格
                Integer productSpecId = buyWeightMachine.getProductSpecId();
                String productSpecName = buyWeightMachine.getProductSpecName();
                productPendingStorageDTO.setContractId(buyWeightMachine.getContractNum())
                        .setMachineId(buyWeightMachine.getMachineId()).setProcessMode(processMode)
                        .setArrivalTime(DateUtils.date2Format(null,"yyyy年MM月dd日 HH:mm:ss",buyWeightMachine.getArrivalTime()))
                        .setAnalysisResult(buyWeightMachine.getAssayResult())
                        .setProductUnit(configProduct.getUnitValue()).setProductId(buyWeightMachine.getProductId())
                        .setProductName(buyWeightMachine.getProductName())
                        .setLicensePlateNumber(buyWeightMachine.getTrainNumber())
                        .setProductSpecId(productSpecId).setProductSpecName(productSpecName);
                productPendingStorageDTOS.add(productPendingStorageDTO);
            });
        }
        return productPendingStorageDTOS;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bulkDiscountedProduct(BulkPendingStorageProductDTO bulkPendingStorageProductDTO, AuthPlatformUserInfo userInfo) {
        if(!ObjectUtils.isEmpty(bulkPendingStorageProductDTO)){
            BigDecimal discountedPrice = bulkPendingStorageProductDTO.getDiscountedPrice();
            //根据磅单号查询地磅数据信息
            List<String> machineIds = bulkPendingStorageProductDTO.getMachineIds();
            List<ProductPendingStorageDTO> productPendingStorageDTOS = transferToPendingStorageDTO(machineIds,EnumStorage.ProcessMode.discount_storage.getKey());
            if(!CollectionUtils.isEmpty(productPendingStorageDTOS)){
                productPendingStorageDTOS.forEach(productPendingStorageDTO -> {
                    //分单条插入待入库的车辆信息，并设置折价后单价
                    saveDiscountedProduct(productPendingStorageDTO,discountedPrice,userInfo);
                });
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bulkOutTonBag(FinishedProductOutListDTO finishedProductOutListDTO, AuthPlatformUserInfo userInfo) {
        if(ObjectUtils.isEmpty(finishedProductOutListDTO)){
           return;
        }
        BulkOutTonBagListDTO bulkOutTonBagListDTO = finishedProductOutListDTO.getBulkOutTonBagListDTO();
        List<FinishedProductOutDTO> finishedProductOutDTOS = finishedProductOutListDTO.getFinishedProductOutDTOS();
        List<String> tonBagNumbers;
        //待出库车辆信息唯一id
        String contractCarId;
        if(!ObjectUtils.isEmpty(bulkOutTonBagListDTO)){
            contractCarId = bulkOutTonBagListDTO.getContractCarId();
        }else {
            contractCarId = finishedProductOutDTOS.get(0).getContractCarId();
        }
        if(!ObjectUtils.isEmpty(bulkOutTonBagListDTO)&&!CollectionUtils.isEmpty(tonBagNumbers = bulkOutTonBagListDTO.getTonBagNumbers()
                .stream().map(TonBagDTO::getTonBagNumber).collect(Collectors.toList()))){
            //准备出库集合
            List<FinishedProductOutStorageDetail> list = new ArrayList<>();
            for (String tonBagNumber: tonBagNumbers) {
                FinishedProductPendingStorage finishedProductPendingStorage = finishedProductPendingStorageMapper.selectOne(
                        new FinishedProductPendingStorage().setTonBagNumber(tonBagNumber).setPutStorage(EnumStorage.PutStorage.storage.getKey())
                                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                                .setStatus(EnumStorage.Status.normal.getKey()));
                if(ObjectUtils.isEmpty(finishedProductPendingStorage)){
                    log.info("销售出库，根据吨袋编号 ： {}， 在finished_product_pending_storage中没有查找到相关入库信息！",tonBagNumber);
                    continue;
                }
                FinishedProductOutStorageDetail finishedProductOutStorageDetail = new FinishedProductOutStorageDetail();
                BeanUtils.copyProperties(finishedProductPendingStorage,finishedProductOutStorageDetail);
                finishedProductOutStorageDetail.setContractCarId(contractCarId).setOutStorageTime(LocalDateTime.now())
                        .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                        .setCreateId(userInfo.getId()).setCreator(userInfo.getName()).setCreateTime(LocalDateTime.now());
                list.add(finishedProductOutStorageDetail);
                //更新库存表
                deductStock(finishedProductPendingStorage.getStorageId()
                        ,finishedProductPendingStorage.getProductId(),finishedProductPendingStorage.getProductSpecId()
                        ,finishedProductPendingStorage.getProductNumber(),userInfo);
            }
            //批量生成出库记录
            finishedProductOutStorageDetailMapper.bulkInsert(list);
        }
        //手动输入的成品出库
        for (FinishedProductOutDTO finishedProductOutDTO:finishedProductOutDTOS){
            //生成出库记录
            FinishedProductFreeOut finishedProductFreeOut = new FinishedProductFreeOut();
            BeanUtils.copyProperties(finishedProductOutDTO,finishedProductFreeOut);
            finishedProductFreeOut.setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                    .setCreateId(userInfo.getId()).setCreator(userInfo.getName()).setCreateTime(LocalDateTime.now()).insert();
            //扣减库存
            deductStock(finishedProductOutDTO.getStorageId()
                    ,finishedProductOutDTO.getProductId(),finishedProductOutDTO.getProductSpecId()
                    ,finishedProductOutDTO.getProductNumber(),userInfo);
        }
        //更新出库车辆信息
        Integer rows = finishedProductBeOutOfStorageMapper.update(new FinishedProductBeOutOfStorage()
                        .setOutStorage(EnumStorage.OutStorageStatus.out_storage.getKey())
                        .setModifier(userInfo.getName()).setModifyId(userInfo.getId()).setModifyTime(LocalDateTime.now())
                , new EntityWrapper<FinishedProductBeOutOfStorage>().eq("contract_car_id", contractCarId)
                        .eq("status",EnumStorage.Status.normal.getKey()));
        if(rows<1){
            throw new BusinessException(ResCodeEnum.SAVE_FAIL,"没有找到出库的车辆信息！");
        }
    }

    @Override
    public List<ProductDTO> selectProductByStorageId(String storageId) {
        List<StorageInventory> storageInventories =
                storageInventoryMapper.selectList(new EntityWrapper<StorageInventory>().eq("storage_id", storageId)
                        .eq("status", EnumStorage.Status.normal.getKey()));
        List<ProductDTO> productDTOS = new ArrayList<>();
        if(!CollectionUtils.isEmpty(storageInventories)){
            storageInventories.forEach(storageInventory -> {
                ProductDTO productDTO = new ProductDTO();
                productDTO.setProductId(storageInventory.getProductId()).setProductName(storageInventory.getProductName());
                productDTOS.add(productDTO);
            });
        }
        return productDTOS;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void pickOutProduct(PickOutProductDTO pickOutProductDTO, AuthPlatformUserInfo userInfo) {
        if(ObjectUtils.isEmpty(pickOutProductDTO)){
            return;
        }
        //设置仓库名
        Integer storageId = pickOutProductDTO.getStorageId();
        ConfigRepository configRepository = configRepositoryMapper.selectById(storageId);
        String storageName = null;
        if(!ObjectUtils.isEmpty(configRepository)){
            storageName = configRepository.getName();
        }
        PickOutStorageDetail pickOutStorageDetail = new PickOutStorageDetail();
        BeanUtils.copyProperties(pickOutProductDTO,pickOutStorageDetail);
        pickOutStorageDetail.setReceiveTime(LocalDateTime.now()).setStorageName(storageName).setStatus(EnumStorage.Status.normal.getKey())
                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                .setCreateId(userInfo.getId()).setCreateTime(LocalDateTime.now()).setCreator(userInfo.getName());
        //添加领用记录
        pickOutStorageDetail.insert();
        //扣减库存
        deductStock(pickOutProductDTO.getStorageId(),pickOutProductDTO.getProductId(),pickOutProductDTO.getProductSpecId()
                ,pickOutProductDTO.getProductNumber(),userInfo);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveProductionOutStorage(ProductionOutStorageDetailDTO productionOutStorageDetailDTO, AuthPlatformUserInfo userInfo) {
        if(ObjectUtils.isEmpty(productionOutStorageDetailDTO)){
            return;
        }
        ProductionOutStorageDetail productionOutStorageDetail = new ProductionOutStorageDetail();
        BeanUtils.copyProperties(productionOutStorageDetailDTO,productionOutStorageDetail);
        productionOutStorageDetail.setCreateId(userInfo.getId()).setCreator(userInfo.getName())
                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                .setOutStorageTime(LocalDateTime.now()).setStatus(EnumStorage.Status.normal.getKey());
        //添加一条生产出库信息
        productionOutStorageDetail.insert();
        //扣减库存
        deductStock(productionOutStorageDetailDTO.getStorageId()
                ,productionOutStorageDetailDTO.getProductId(),productionOutStorageDetailDTO.getProductSpecId()
                ,productionOutStorageDetailDTO.getProductNumber(),userInfo);
    }

    @Override
    public ResponseResult<List<StorageManagerListDTO>> selectStorageManagerList(Integer productType,String productName,Pagination pagination
            ,AuthPlatformUserInfo userInfo) {
        Integer factoryId = userInfo.getFactoryId();
        Integer orgId = userInfo.getOrgId();
        //1、成品 2、主料 3、辅料 4、其他
        String typeName;
        switch (productType){
            case 1:
                typeName = "成品";
                break;
            case 2:
                typeName = "主料";
                break;
            case 3:
                typeName = "辅料";
                break;
            default:
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS
                        ,storageInventoryMapper.selectOtherStorageManagerList(pagination,productName,factoryId,orgId)
                        ,PageUtils.transToPage(pagination));
        }
        List<StorageManagerListDTO> storageManagerListDTOS =
                storageInventoryMapper.selectStorageManagerList(pagination, typeName, productName, factoryId, orgId);
        if(productType.equals(1)){
            //查询无规格产品数量
            BigDecimal noSpecProductNum = finishedProductPendingStorageMapper.selectNoSpecProduct(factoryId,orgId);
            storageManagerListDTOS.forEach(storageManagerListDTO -> storageManagerListDTO.setNoSpecProductNum(noSpecProductNum));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,storageManagerListDTOS,PageUtils.transToPage(pagination));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void revisionStorage(RevisionStorageDTO revisionStorageDTO, AuthPlatformUserInfo userInfo) {
        if(ObjectUtils.isEmpty(revisionStorageDTO)){
            return;
        }
        RevisionStorageDetail revisionStorageDetail = new RevisionStorageDetail();
        BeanUtils.copyProperties(revisionStorageDTO,revisionStorageDetail);
        //调整数量
        BigDecimal revisionNumber = revisionStorageDTO.getReviseProductNumber().subtract(revisionStorageDTO.getCurrentProductNumber());
        revisionStorageDetail.setReviseAmount(revisionNumber).setStatus(EnumStorage.Status.normal.getKey())
                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                .setCreateId(userInfo.getId()).setCreateTime(LocalDateTime.now()).setCreator(userInfo.getName());
        //添加一条盘库记录
        revisionStorageDetail.insert();
        //修改库存
        updateStorageInventoryAmount(revisionStorageDTO.getStorageId(),revisionStorageDTO.getProductId(),revisionStorageDTO.getProductSpecId()
                ,revisionStorageDTO.getReviseProductNumber());
    }

    @Override
    public List<RevisionStorageRecordDTO> selectRevisionStorageRecords(String storageId, String productId) {
        List<RevisionStorageDetail> revisionStorageDetails = revisionStorageDetailMapper.selectList(
                new EntityWrapper<RevisionStorageDetail>().eq("product_id", productId).eq("storage_id",storageId)
        );
        List<RevisionStorageRecordDTO> result = new ArrayList<>();
        if(!CollectionUtils.isEmpty(revisionStorageDetails)){
            revisionStorageDetails.forEach(revisionStorageDetail -> {
                RevisionStorageRecordDTO revisionStorageRecordDTO = new RevisionStorageRecordDTO();
                BeanUtils.copyProperties(revisionStorageDetail,revisionStorageRecordDTO);
                revisionStorageRecordDTO.setRevisionTime(LocalDateUtils.formatDate(revisionStorageDetail.getCreateTime(),"yyyy-MM-dd"));
                result.add(revisionStorageRecordDTO);
            });
        }
        return result;
    }

    @Override
    public List<BalanceStorageDTO> selectBalanceStorages(AuthPlatformUserInfo userInfo) {
        //查询所有为负数的库存
        List<StorageInventory> storageInventories = storageInventoryMapper.selectList(
                new EntityWrapper<StorageInventory>().lt("product_number",0)
                        .eq("factory_id",userInfo.getFactoryId()).eq("org_id",userInfo.getOrgId())
                        .eq("status",EnumStorage.Status.normal.getKey())
        );
        List<BalanceStorageDTO> balanceStorageDTOS = new ArrayList<>();
        if(!CollectionUtils.isEmpty(storageInventories)){
            storageInventories.forEach(storageInventory -> {
                BalanceStorageDTO balanceStorageDTO = new BalanceStorageDTO();
                BeanUtils.copyProperties(storageInventory,balanceStorageDTO);
                //将库存数量转换成正数
                balanceStorageDTO.setAmount(storageInventory.getProductNumber().abs());
                balanceStorageDTOS.add(balanceStorageDTO);
            });
        }
        return balanceStorageDTOS;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void balanceStorages(AuthPlatformUserInfo userInfo) {
        //添加一键平库记录
        List<StorageInventory> storageInventories = storageInventoryMapper.selectList(
                new EntityWrapper<StorageInventory>().lt("product_number", 0)
                .eq("factory_id", userInfo.getFactoryId()).eq("org_id", userInfo.getOrgId())
                .eq("status", EnumStorage.Status.normal.getKey()));
        if(!CollectionUtils.isEmpty(storageInventories)){
            List<RevisionStorageDetail> revisionStorageDetails = new ArrayList<>();
            storageInventories.forEach(storageInventory -> {
                RevisionStorageDetail revisionStorageDetail = new RevisionStorageDetail();
                revisionStorageDetail.setOrgId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId())
                        .setProductId(storageInventory.getProductId()).setProductName(storageInventory.getProductName())
                        .setProductUnit(storageInventory.getProductUnit())
                        .setProductSpecId(storageInventory.getProductSpecId()).setProductSpecName(storageInventory.getProductSpecName())
                        .setStorageId(storageInventory.getStorageId()).setStorageName(storageInventory.getStorageName())
                        .setCurrentProductNumber(storageInventory.getProductNumber()).setReviseProductNumber(BigDecimal.ZERO)
                        .setReviseAmount(storageInventory.getProductNumber().abs())
                        .setStatus(EnumStorage.Status.normal.getKey()).setReason("一键平库")
                        .setCreateTime(LocalDateTime.now()).setCreator(userInfo.getName()).setCreateId(userInfo.getId());
                revisionStorageDetails.add(revisionStorageDetail);
                storageInventory.setProductNumber(BigDecimal.ZERO);
            });
            storageInventoryService.updateStorageInventories(storageInventories);
            //添加记录
            revisionStorageDetailService.insertRevisionStorageDetails(revisionStorageDetails);
            log.info("一键平库的更新的总条数: {}",storageInventories.size());
        }
    }

    @Override
    public StorageManagerRecordListDTO selectInAndOutStorageRecords(Integer storageId, Integer productId,Integer productSpecId) {
        StorageManagerRecordListDTO storageManagerRecordListDTO = new StorageManagerRecordListDTO();
        //查询出库记录
        List<InAndOutStorageRecordDTO> outStorageRecords = getOutStorageRecordsDetail(storageId,productId,productSpecId);
        //查询入库记录
        List<InAndOutStorageRecordDTO> inStorageRecords = getInStorageRecordsDetail(storageId,productId,productSpecId);
        storageManagerRecordListDTO.setInStorageRecords(inStorageRecords).setOutStorageRecords(outStorageRecords);
        return storageManagerRecordListDTO;
    }

    @Override
    public InStorageRecordListDTO getInStorageDetail(AuthPlatformUserInfo userInfo) {
        Integer factoryId = userInfo.getFactoryId();
        Integer orgId = userInfo.getOrgId();
        //采购入库记录
        List<BuyInStorageRecordDTO> buyInStorageRecordDTOS = selectBuyInStorageRecord(factoryId,orgId);
        //产成品入库记录
        List<FinishProductInStorageRecordDTO> finishProductInStorageRecordDTOS = selectFinishProductInStorageRecord(factoryId,orgId);
        //新增入库记录
        List<FreeInStorageRecordDTO> freeInStorageRecordDTOS = selectFreeInStorageRecord(factoryId,orgId);
        InStorageRecordListDTO inStorageRecordListDTO = new InStorageRecordListDTO();
        inStorageRecordListDTO.setBuyInStorageRecordDTOS(buyInStorageRecordDTOS)
                .setFinishProductInStorageRecordDTOS(finishProductInStorageRecordDTOS)
                .setFreeInStorageRecordDTOS(freeInStorageRecordDTOS);
        return inStorageRecordListDTO;
    }

    @Override
    public OutStorageRecordListDTO getOutStorageRecordDetail(AuthPlatformUserInfo userInfo) {
        Integer factoryId = userInfo.getFactoryId();
        Integer orgId = userInfo.getOrgId();
        OutStorageRecordListDTO outStorageRecordListDTO = new OutStorageRecordListDTO();
        //销售出库
        List<SaleOutStorageRecordDTO> saleOutStorageRecordDTOS = selectSaleOutStorageRecord(factoryId,orgId);
        //领用出库
        List<PickOutStorageDTO> pickOutStorageDTOS = selectPickOutStorage(factoryId,orgId);
        //生产出库
        List<ProductionOutStorageDTO> productionOutStorageDTOS = selectProductionOutStorage(factoryId,orgId);
        outStorageRecordListDTO.setPickOutStorageDTOS(pickOutStorageDTOS).setProductionOutStorageDTOS(productionOutStorageDTOS)
                .setSaleOutStorageRecordDTOS(saleOutStorageRecordDTOS);
        return outStorageRecordListDTO;
    }

    @Override
    public FinishedProductInStorageDetailDTO getFinishedProductInStorageDetail(String furnaceNumber, String furnaceTimes
            , String scheduling, String storageId,String storageTime) {
        List<FinishedProductPendingStorage> finishedProductPendingStorages =
                finishedProductPendingStorageMapper.selectList(new EntityWrapper<FinishedProductPendingStorage>()
                        .eq("put_storage", EnumStorage.PutStorage.storage.getKey()).eq("furnace_number",furnaceNumber)
                        .eq("furnace_times",furnaceTimes).eq("scheduling",scheduling).eq("storage_id",storageId)
                        .addFilter("DATE_FORMAT(storage_time,'%Y-%m-%d') = '"+storageTime+"'"));
        FinishedProductInStorageDetailDTO finishedProductInStorageDetailDTO = new FinishedProductInStorageDetailDTO();
        if(!CollectionUtils.isEmpty(finishedProductPendingStorages)){
            String inStorageTime = LocalDateUtils.formatDate(finishedProductPendingStorages.get(0).getStorageTime(), "yyyy-MM-dd");
            finishedProductInStorageDetailDTO.setInStorageTime(inStorageTime);
            BigDecimal productNumber =
                    finishedProductPendingStorages.stream().map(FinishedProductPendingStorage::getProductNumber).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
            finishedProductInStorageDetailDTO.setProductNumber(productNumber);
            List<InStorageTonBagDTO> inStorageTonBagDTOS = new ArrayList<>();
            finishedProductPendingStorages.forEach(finishedProductPendingStorage -> {
                InStorageTonBagDTO inStorageTonBagDTO = new InStorageTonBagDTO();
                inStorageTonBagDTO.setInStorageTime(inStorageTime)
                        .setProductNumber(finishedProductPendingStorage.getProductNumber())
                        .setTonBagNumber(finishedProductPendingStorage.getTonBagNumber());
                inStorageTonBagDTOS.add(inStorageTonBagDTO);
            });
            finishedProductInStorageDetailDTO.setInStorageTonBagDTOS(inStorageTonBagDTOS);
        }
        return finishedProductInStorageDetailDTO;
    }

    @Override
    public List<SaleOutStorageDetailDTO> getSaleOutStorageRecordDetail(String contractId,Integer storageId) {
        //根据合同号查询所有出库车辆信息
        List<FinishedProductBeOutOfStorage> finishedProductBeOutOfStorages =
                finishedProductBeOutOfStorageMapper.selectByContractAndStorage(contractId,storageId);
        //查询新增出库车辆
        List<FinishedProductBeOutOfStorage> freeFinishedProductBeOutOfStorages =
                finishedProductBeOutOfStorageMapper.selectFreeOutByContractAndStorage(contractId,storageId);
        freeFinishedProductBeOutOfStorages.addAll(finishedProductBeOutOfStorages);
        List<SaleOutStorageDetailDTO> saleOutStorageDetailDTOS = new ArrayList<>();
        if(!CollectionUtils.isEmpty(freeFinishedProductBeOutOfStorages)){
            List<FinishedProductBeOutOfStorage> result =
                    freeFinishedProductBeOutOfStorages.stream().collect(Collectors.collectingAndThen(
                            Collectors.toCollection(() -> new TreeSet<>(Comparator.comparing(FinishedProductBeOutOfStorage::getContractCarId)))
                            ,ArrayList::new));
            result.forEach(finishedProductBeOutOfStorage -> {
                SaleOutStorageDetailDTO saleOutStorageDetailDTO = new SaleOutStorageDetailDTO();
                saleOutStorageDetailDTO.setContractId(contractId)
                        .setLicensePlateNumber(finishedProductBeOutOfStorage.getLicensePlateNumber())
                        .setProductName(finishedProductBeOutOfStorage.getProductName())
                        .setProductUnit(finishedProductBeOutOfStorage.getProductUnit());
                //查询改车辆的吨袋信息
                List<SaleOutStorageTonBagDTO> saleOutStorageTonBagDTOS =
                        finishedProductOutStorageDetailMapper.selectSaleOutStorageTonBagDTOById(finishedProductBeOutOfStorage.getContractCarId());
                saleOutStorageDetailDTO.setSaleOutStorageTonBagDTOS(saleOutStorageTonBagDTOS);
                //查询质检信息
                List<SampleQualityTestDTO> sampleQualityTestDTOS =
                        finishedProductOutStorageDetailMapper.selectSampleInfo(finishedProductBeOutOfStorage.getContractCarId());
                //查询每个样品的质检结果
                if(!CollectionUtils.isEmpty(sampleQualityTestDTOS)){
                    sampleQualityTestDTOS.forEach(sampleQualityTestDTO -> {
                        String sampleCode = sampleQualityTestDTO.getSampleCode();
                        List<SampleAssayResult> sampleAssayResults =
                                sampleAssayResultMapper.selectList(new EntityWrapper<SampleAssayResult>()
                                        .eq("sample_code",sampleCode).eq("type",1));
                        List<AssayItem> qualityTestResult = new ArrayList<>();
                        if(!CollectionUtils.isEmpty(sampleAssayResults)){
                            sampleAssayResults.forEach(sampleAssayResult -> {
                                AssayItem assayItem = new AssayItem();
                                assayItem.setAssayItem(sampleAssayResult.getAssayItem())
                                        .setAssayValue(sampleAssayResult.getAssayValue())
                                        .setAssayValueUnit(getStorageEnumByCode(EnumSampleRelation.ProductUnit.class
                                                , "getKey", sampleAssayResult.getTestUnit()).getValue());
                                qualityTestResult.add(assayItem);
                                sampleQualityTestDTO.setQualityTestResult(qualityTestResult);
                            });
                        }
                    });
                }
                //查询新增出库的信息
                FinishedProductFreeOutDetailDTO finishedProductFreeOutDetailDTO =
                        finishedProductFreeOutMapper.selectByCarId(finishedProductBeOutOfStorage.getContractCarId(),storageId);
                saleOutStorageDetailDTO.setSampleQualityTestDTOS(sampleQualityTestDTOS);
                saleOutStorageDetailDTO.setFinishedProductFreeOutDetailDTO(finishedProductFreeOutDetailDTO);
                saleOutStorageDetailDTOS.add(saleOutStorageDetailDTO);
            });
        }
        return saleOutStorageDetailDTOS;
    }

    @Override
    public List<BuyInStorageDetailDTO> getBuyInStorageDetail(String contractId, Integer storageId) {
        List<BuyGoodsPendingStorage> buyGoodsPendingStorages =
                buyGoodsPendingStorageMapper.selectList(new EntityWrapper<BuyGoodsPendingStorage>().eq("contract_id",contractId)
                        .eq("storage_id",storageId).eq("status",EnumStorage.Status.normal.getKey()));
        List<BuyInStorageDetailDTO> buyInStorageDetailDTOS = new ArrayList<>();
        if(!CollectionUtils.isEmpty(buyGoodsPendingStorages)){
            buyGoodsPendingStorages.forEach(buyGoodsPendingStorage -> {
                BuyInStorageDetailDTO buyInStorageDetailDTO = new BuyInStorageDetailDTO();
                buyInStorageDetailDTO.setProductName(buyGoodsPendingStorage.getProductName())
                        .setProductNumber(buyGoodsPendingStorage.getProductNumber())
                        .setProductSpecName(buyGoodsPendingStorage.getProductSpecName())
                        .setLicensePlateNumber(buyGoodsPendingStorage.getLicensePlateNumber())
                        .setQualityTestResult(getStorageEnumByCode(EnumStorage.AnalysisResult.class
                                , GET_CODE_METHOD, buyGoodsPendingStorage.getAnalysisResult()).getValue())
                        .setRemark(buyGoodsPendingStorage.getRemark())
                        .setInStorageTime(LocalDateUtils.formatDate(buyGoodsPendingStorage.getStorageTime(),"yyyy-MM-dd"));
                String machineId = buyGoodsPendingStorage.getMachineId();
                //查询磅单号相关的样品
                List<BuySample> buySamples = buySampleMapper.selectSampleByMachineId(machineId);
                List<InStorageSampleDTO> inStorageSampleDTOS = new ArrayList<>();
                if(!CollectionUtils.isEmpty(buySamples)){
                    buySamples.forEach(buySample -> {
                        String sampleCode = buySample.getSampleCode();
                        InStorageSampleDTO inStorageSampleDTO = new InStorageSampleDTO();
                        inStorageSampleDTO.setSampleCode(sampleCode);
                        List<SampleAssayResult> sampleAssayResults =
                                sampleAssayResultMapper.selectList(new EntityWrapper<SampleAssayResult>()
                                        .eq("sample_code", sampleCode).eq("type",1)
                                        .eq("status",EnumStorage.Status.normal.getKey()));
                        List<AssayItem> assayItems = new ArrayList<>();
                        if(!CollectionUtils.isEmpty(sampleAssayResults)){
                            sampleAssayResults.forEach(sampleAssayResult -> {
                                AssayItem assayItem = new AssayItem();
                                assayItem.setAssayItem(sampleAssayResult.getAssayItem())
                                        .setAssayValueUnit(getStorageEnumByCode(EnumSampleRelation.ProductUnit.class
                                                , "getKey", sampleAssayResult.getTestUnit()).getValue())
                                        .setAssayValue(sampleAssayResult.getAssayValue());
                                assayItems.add(assayItem);
                            });
                        }
                        inStorageSampleDTO.setAssayItems(assayItems);
                        inStorageSampleDTOS.add(inStorageSampleDTO);
                    });
                }
                buyInStorageDetailDTO.setInStorageSampleDTOS(inStorageSampleDTOS);
                buyInStorageDetailDTOS.add(buyInStorageDetailDTO);
            });
        }
        return buyInStorageDetailDTOS;
    }

    @Override
    public TonBagDTO selectInStorageTonBagInfo(String tonBagNumber,AuthPlatformUserInfo userInfo) {
        FinishedProductPendingStorage finishedProductPendingStorage = finishedProductPendingStorageMapper.selectOne(
                new FinishedProductPendingStorage().setTonBagNumber(tonBagNumber)
                        .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                .setPutStorage(EnumStorage.PutStorage.not_storage.getKey()).setStatus(EnumStorage.Status.normal.getKey()));
        TonBagDTO tonBagDTO = new TonBagDTO();
        if(!ObjectUtils.isEmpty(finishedProductPendingStorage)){
            tonBagDTO.setTonBagNumber(tonBagNumber)
                    .setProductNumber(finishedProductPendingStorage.getProductNumber())
                    .setProductUnit(finishedProductPendingStorage.getProductUnit());
        }
        return tonBagDTO;
    }

    @Override
    public List<StorageProductSpecDTO> selectSaleProductSpec(AuthPlatformUserInfo userInfo,Integer productId) {
        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        List<StorageInventory> storageInventories =
                storageInventoryMapper.selectList(new EntityWrapper<StorageInventory>().eq("product_id", productId)
                        .eq("factory_id ",factoryId).eq("org_id",orgId)
                        .eq("status",EnumStorage.Status.normal.getKey()));
        List<StorageProductSpecDTO> storageProductSpecDTOS = new ArrayList<>();
        storageInventories.forEach(storageInventory -> {
            StorageProductSpecDTO storageProductSpecDTO =new StorageProductSpecDTO();
            storageProductSpecDTO.setProductSpecId(storageInventory.getProductSpecId())
                    .setProductSpecName(storageInventory.getProductSpecName());
            storageProductSpecDTOS.add(storageProductSpecDTO);
        });
        return storageProductSpecDTOS;
    }

    private List<ProductionOutStorageDTO> selectProductionOutStorage(Integer factoryId,Integer orgId) {
        return inAndOutStorageRecordMapper.selectProductionOutStorage(factoryId,orgId);
    }

    private List<PickOutStorageDTO> selectPickOutStorage(Integer factoryId,Integer orgId) {
        return inAndOutStorageRecordMapper.selectPickOutStorage(factoryId,orgId);
    }

    private List<SaleOutStorageRecordDTO> selectSaleOutStorageRecord(Integer factoryId,Integer orgId) {
        //吨袋销售出库
        List<SaleOutStorageRecordDTO> saleOutStorageRecordDTOS = inAndOutStorageRecordMapper.selectSaleOutStorageRecord(factoryId, orgId);
        //新增销售出库
        List<SaleOutStorageRecordDTO> saleFreeOutStorageRecordDTOS = inAndOutStorageRecordMapper.selectSaleFreeOutStorageRecord(factoryId, orgId);
        saleFreeOutStorageRecordDTOS.addAll(saleOutStorageRecordDTOS);
        List<SaleOutStorageRecordDTO> result = new ArrayList<>();
        for(SaleOutStorageRecordDTO saleOutStorageRecordDTO:saleFreeOutStorageRecordDTOS){
            boolean flag = false;
            for(SaleOutStorageRecordDTO saleOutStorageRecordDTO1:result){
                if(saleOutStorageRecordDTO1.getContractId().equals(saleOutStorageRecordDTO.getContractId())
                        &&saleOutStorageRecordDTO1.getStorageId().equals(saleOutStorageRecordDTO.getStorageId())){
                    //时间最大值、最小值
                    String time = getSaleFreeProductTime(saleOutStorageRecordDTO1.getTime(),saleOutStorageRecordDTO.getTime());
                    saleOutStorageRecordDTO1.setTonBagQuantity(saleOutStorageRecordDTO1.getTonBagQuantity()+saleOutStorageRecordDTO.getTonBagQuantity())
                            .setTotalQuantity(saleOutStorageRecordDTO1.getTotalQuantity().add(saleOutStorageRecordDTO.getTotalQuantity()))
                            .setTime(time);
                    flag = true;
                    break;
                }
            }
            if(!flag){
                result.add(saleOutStorageRecordDTO);
            }
        }
        return result;
    }

    private String getSaleFreeProductTime(String time1,String time2){
        String split = " 至 ";
        String[] times1 = time1.split(split);
        String[] times2 = time2.split(split);
        String minTime = times1[0];
        String maxTime = times1[1];
        StringBuilder sb = new StringBuilder();
        if(minTime.compareTo(times2[0])>=0){
            minTime = times2[0];
        }
        if(maxTime.compareTo(times2[1])<=0){
            maxTime = times2[1];
        }
        sb.append(minTime);
        sb.append(split);
        sb.append(maxTime);
        return sb.toString();
    }

    private List<FinishProductInStorageRecordDTO> selectFinishProductInStorageRecord(Integer factoryId,Integer orgId) {
        return inAndOutStorageRecordMapper.selectFinishProductInStorageRecord(factoryId,orgId);
    }

    private List<FreeInStorageRecordDTO> selectFreeInStorageRecord(Integer factoryId,Integer orgId) {
        return inAndOutStorageRecordMapper.selectFreeInStorageRecord(factoryId,orgId);
    }

    private List<BuyInStorageRecordDTO> selectBuyInStorageRecord(Integer factoryId,Integer orgId) {
        return inAndOutStorageRecordMapper.selectBuyInStorageRecord(factoryId,orgId);
    }

    private List<FreeInStorageRecordDTO> selectFreeInStorageRecordWithPage(Integer factoryId,Integer orgId,Pagination pagination) {
        return inAndOutStorageRecordMapper.selectFreeInStorageRecordWithPage(factoryId,orgId,pagination);
    }
    
    /**
     * 查询库存管理入库记录
     * @param storageId a
     * @param productId b
     * @param productSpecId c
     * @return d
     */
    private List<InAndOutStorageRecordDTO> getInStorageRecordsDetail(Integer storageId, Integer productId,Integer productSpecId) {
        List<InAndOutStorageRecordDTO> inAndOutStorageRecordDTOS = new ArrayList<>();
        //采购入库记录
        List<BuyGoodsPendingStorage> buyGoodsPendingStorages =
                buyGoodsPendingStorageMapper.selectList(new EntityWrapper<BuyGoodsPendingStorage>()
                        .eq("product_id", productId).eq("storage_id", storageId).eq("product_spec_id",productSpecId)
                        .eq("put_storage",EnumStorage.PutStorage.storage.getKey()));
        if(!CollectionUtils.isEmpty(buyGoodsPendingStorages)){
            buyGoodsPendingStorages.forEach(buyGoodsPendingStorage -> {
                InAndOutStorageRecordDTO inAndOutStorageRecordDTO = new InAndOutStorageRecordDTO();
                inAndOutStorageRecordDTO.setTime(LocalDateUtils.formatDate(buyGoodsPendingStorage.getStorageTime(),"yyyy-MM-dd"))
                        .setCreator(buyGoodsPendingStorage.getModifier())
                        .setStorageName(buyGoodsPendingStorage.getStorageName()).setProductNumber(buyGoodsPendingStorage.getProductNumber());
                inAndOutStorageRecordDTOS.add(inAndOutStorageRecordDTO);
            });
        }
        //销售入库记录
        List<FinishedProductPendingStorage> finishedProductPendingStorages =
                finishedProductPendingStorageMapper.selectList(new EntityWrapper<FinishedProductPendingStorage>()
                        .eq("storage_id",storageId).eq("product_id",productId).eq("product_spec_id",productSpecId)
                        .eq("put_storage",EnumStorage.PutStorage.storage.getKey()).eq("status",EnumStorage.Status.normal.getKey()));
        if(!CollectionUtils.isEmpty(finishedProductPendingStorages)){
            for(FinishedProductPendingStorage finishedProductPendingStorage:finishedProductPendingStorages){
                InAndOutStorageRecordDTO inAndOutStorageRecordDTO = new InAndOutStorageRecordDTO();
                ConfigRepository configRepository = configRepositoryMapper.selectById(finishedProductPendingStorage.getStorageId());
                if(ObjectUtils.isEmpty(configRepository)){
                    continue;
                }
                inAndOutStorageRecordDTO.setTime(LocalDateUtils.formatDate(finishedProductPendingStorage.getStorageTime(),"yyyy-MM-dd"))
                        .setCreator(finishedProductPendingStorage.getModifier())
                        .setStorageName(configRepository.getName()).setProductNumber(finishedProductPendingStorage.getProductNumber());
                inAndOutStorageRecordDTOS.add(inAndOutStorageRecordDTO);
            }
        }
        //新增入库
        List<FreeStorageDetail> freeStorageDetails =
                freeStorageDetailMapper.selectList(new EntityWrapper<FreeStorageDetail>()
                        .eq("product_id", productId).and().eq("storage_id", storageId)
                        .eq("product_spec_id",productSpecId));
        if(!CollectionUtils.isEmpty(freeStorageDetails)){
            freeStorageDetails.forEach(freeStorageDetail -> {
                InAndOutStorageRecordDTO inAndOutStorageRecordDTO = new InAndOutStorageRecordDTO();
                inAndOutStorageRecordDTO.setTime(LocalDateUtils.formatDate(freeStorageDetail.getStorageTime(),"yyyy-MM-dd"))
                        .setCreator(freeStorageDetail.getCreator())
                        .setStorageName(freeStorageDetail.getStorageName()).setProductNumber(freeStorageDetail.getProductNumber());
                inAndOutStorageRecordDTOS.add(inAndOutStorageRecordDTO);
            });
        }
        return inAndOutStorageRecordDTOS;
    }

    private List<InAndOutStorageRecordDTO> getOutStorageRecordsDetail(Integer storageId, Integer productId,Integer productSpecId) {
        List<InAndOutStorageRecordDTO> inAndOutStorageRecordDTOS = new ArrayList<>();
        //查询生产出库记录
        List<ProductionOutStorageDetail> productionOutStorageDetails = productionOutStorageDetailMapper.selectList(
                new EntityWrapper<ProductionOutStorageDetail>().eq("storage_id",storageId)
                        .eq("product_id",productId).eq("product_spec_id",productSpecId)
                        .eq("status",EnumStorage.Status.normal.getKey())
        );
        if(!CollectionUtils.isEmpty(productionOutStorageDetails)){
            productionOutStorageDetails.forEach(productionOutStorageDetail -> {
                //出库时间
                LocalDateTime outStorageTime = productionOutStorageDetail.getOutStorageTime();
                String timeStr = LocalDateUtils.formatDate(outStorageTime, "yyyy-MM-dd");
                //出库数量
                BigDecimal productNumber = productionOutStorageDetail.getProductNumber();
                //仓库名字
                String storageName = productionOutStorageDetail.getStorageName();
                //出库人
                String creator = productionOutStorageDetail.getCreator();
                InAndOutStorageRecordDTO inAndOutStorageRecordDTO =
                        setInAndOutStorageRecordDTOValue(timeStr,productNumber,storageName,creator);
                inAndOutStorageRecordDTOS.add(inAndOutStorageRecordDTO);
            });
        }
        //查询销售出库记录
        List<InAndOutStorageRecordDTO> saleOutStorageRecord =
                finishedProductBeOutOfStorageMapper.selectSaleOutStorageRecord(storageId,productId,productSpecId);
        if(!CollectionUtils.isEmpty(saleOutStorageRecord)){
            inAndOutStorageRecordDTOS.addAll(saleOutStorageRecord);
        }
        //销售出库新增出库
        List<FinishedProductFreeOut> finishedProductFreeOuts =
                finishedProductFreeOutMapper.selectList(new EntityWrapper<FinishedProductFreeOut>()
                        .eq("product_id",productId).eq("product_spec_id",productSpecId).eq("storage_id",storageId));
        finishedProductFreeOuts.forEach(finishedProductFreeOut -> {
            InAndOutStorageRecordDTO inAndOutStorageRecordDTO = new InAndOutStorageRecordDTO();
            inAndOutStorageRecordDTO.setProductNumber(finishedProductFreeOut.getProductNumber())
                    .setStorageName(finishedProductFreeOut.getStorageName()).setCreator(finishedProductFreeOut.getCreator())
                    .setTime(LocalDateUtils.formatDate(finishedProductFreeOut.getCreateTime(),"yyyy-MM-dd"));
            inAndOutStorageRecordDTOS.add(inAndOutStorageRecordDTO);
        });
        //查询自由出库记录
        List<PickOutStorageDetail> pickOutStorageDetails = pickOutStorageDetailMapper.selectList(new EntityWrapper<PickOutStorageDetail>()
                .eq("product_id", productId).eq("storage_id", storageId).eq("product_spec_id",productSpecId));
        if(!CollectionUtils.isEmpty(pickOutStorageDetails)){
            pickOutStorageDetails.forEach(pickOutStorageDetail -> {
                InAndOutStorageRecordDTO inAndOutStorageRecordDTO = new InAndOutStorageRecordDTO();
                inAndOutStorageRecordDTO.setTime(LocalDateUtils.formatDate(pickOutStorageDetail.getReceiveTime(),"yyyy-MM-dd"))
                        .setCreator(pickOutStorageDetail.getReceiver())
                        .setStorageName(pickOutStorageDetail.getStorageName()).setProductNumber(pickOutStorageDetail.getProductNumber());
                inAndOutStorageRecordDTOS.add(inAndOutStorageRecordDTO);
            });
        }
        return inAndOutStorageRecordDTOS;
    }

    private InAndOutStorageRecordDTO setInAndOutStorageRecordDTOValue(String timeStr, BigDecimal productNumber, String storageName, String creator) {
        InAndOutStorageRecordDTO inAndOutStorageRecordDTO = new InAndOutStorageRecordDTO();
        inAndOutStorageRecordDTO.setTime(timeStr).setProductNumber(productNumber).setStorageName(storageName).setCreator(creator);
        return inAndOutStorageRecordDTO;
    }

    /**
     * 修改仓库库存数量
     * @param storageId 仓库id
     * @param productId  产品id
     * @param reviseProductNumber 产品数量
     */
    private synchronized void updateStorageInventoryAmount(Integer storageId, Integer productId, Integer productSpecId
            ,BigDecimal reviseProductNumber) {
        //根据仓库id、产品id、规格id查询库存
        StorageInventory storageInventory = selectStorageInventory(storageId, productId, productSpecId);
        //修改库存数量
        storageInventory.setProductNumber(reviseProductNumber).updateById();
    }

    private StorageInventory selectStorageInventory(Integer storageId, Integer productId, Integer productSpecId){
        StorageInventory storageInventory = storageInventoryMapper.selectOne(new StorageInventory()
                .setProductId(productId).setStorageId(storageId).setProductSpecId(productSpecId)
                .setStatus(EnumStorage.Status.normal.getKey()));
        if(ObjectUtils.isEmpty(storageInventory)){
            throw new BusinessException(ResCodeEnum.MISS_NECESSARY_PARAM, ExceptionMessageEnum.PRODUCT_STORAGE_NOT_EXIST);
        }
        return storageInventory;
    }

    private void saveDiscountedProduct(ProductPendingStorageDTO productPendingStorageDTO, BigDecimal discountedPrice, AuthPlatformUserInfo userInfo) {
        if(!ObjectUtils.isEmpty(productPendingStorageDTO)){
            //新增待入库的车辆等信息
            savePendingStorageProduct(productPendingStorageDTO,userInfo);
            //更新折价单价
            buyWeightMachineService.updateDiscountUnitPrice(productPendingStorageDTO.getMachineId(),discountedPrice,userInfo);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void savePendingStorageProduct(ProductPendingStorageDTO productPendingStorageDTO, AuthPlatformUserInfo userInfo) {
        if(ObjectUtils.isEmpty(productPendingStorageDTO)){
            throw new BusinessException(ResCodeEnum.MISS_NECESSARY_PARAM, ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        String machineId = productPendingStorageDTO.getMachineId();
        //根据榜单号查询货物重量
        BuyWeightMachine buyWeightMachine = getBuyWeiMachineById(machineId);
        BigDecimal productNumber = buyWeightMachine.getNetWeight();
        BuyGoodsPendingStorage buyGoodsPendingStorage = new BuyGoodsPendingStorage();
        BeanUtils.copyProperties(productPendingStorageDTO,buyGoodsPendingStorage);
        String buyProductPendingStorageId = UUID.randomUUID().toString().replace("-","");
        //车次货物确认扣重
        BigDecimal carDeductWeight = Optional.ofNullable(buyWeightMachine.getCarDeductWeight()).orElse(BigDecimal.ZERO);
        buyGoodsPendingStorage.setBuyProductPendingStorageId(buyProductPendingStorageId).setProductNumber(productNumber.subtract(carDeductWeight))
                .setRemark(buyWeightMachine.getRemark())
                .setStatus(EnumStorage.Status.normal.getKey())
                .setArrivalTime(LocalDateUtils.parseDate(productPendingStorageDTO.getArrivalTime(),"yyyy年MM月dd日 HH:mm:ss"))
                .setPutStorage(EnumStorage.PutStorage.not_storage.getKey())
                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                .setCreateId(userInfo.getId()).setCreator(userInfo.getName()).setCreateTime(LocalDateTime.now());
        buyGoodsPendingStorage.insert();
    }

    @Override
    public ResponseResult<List<BuyPendingStorageProductListDTO>> selectBuyPendingStorageInfo(Pagination pagination,AuthPlatformUserInfo userInfo) {
        List<BuyGoodsPendingStorage> buyGoodsPendingStorages =
                buyGoodsPendingStorageMapper.selectPage(pagination,new EntityWrapper<BuyGoodsPendingStorage>()
                .eq("put_storage", EnumStorage.PutStorage.not_storage.getKey())
                .eq("factory_id",userInfo.getFactoryId()).eq("org_id",userInfo.getOrgId())
                .eq("status", EnumStorage.Status.normal.getKey()).orderDesc(Arrays.asList("create_time")));
        List<BuyPendingStorageProductListDTO> buyPendingStorageProductListDTOS = new ArrayList<>();
        if(!CollectionUtils.isEmpty(buyGoodsPendingStorages)){
            buyGoodsPendingStorages.forEach(buyGoodsPendingStorage -> {
                BuyPendingStorageProductListDTO buyPendingStorageProductListDTO = new BuyPendingStorageProductListDTO();
                BeanUtils.copyProperties(buyGoodsPendingStorage,buyPendingStorageProductListDTO);
                Integer analysisResult = buyGoodsPendingStorage.getAnalysisResult();
                EnumStorage.AnalysisResult analysisResultEnum =
                        getStorageEnumByCode(EnumStorage.AnalysisResult.class, GET_CODE_METHOD, analysisResult);
                Integer processMode = buyGoodsPendingStorage.getProcessMode();
                EnumStorage.ProcessMode processModeEnum =
                        getStorageEnumByCode(EnumStorage.ProcessMode.class, GET_CODE_METHOD, processMode);
                buyPendingStorageProductListDTO.setAnalysisTime(
                        LocalDateUtils.formatDate(buyGoodsPendingStorage.getArrivalTime(),"yyyy年MM月dd日 HH:mm:ss")
                ).setAnalysisResultName(analysisResultEnum.getValue()).setProcessModeName(processModeEnum.getValue());
                buyPendingStorageProductListDTOS.add(buyPendingStorageProductListDTO);
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,buyPendingStorageProductListDTOS, PageUtils.transToPage(pagination));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveFreeStorage(FreeStorageDetailDTO freeStorageDetailDTO, AuthPlatformUserInfo userInfo) {
        if(ObjectUtils.isEmpty(freeStorageDetailDTO)){
            throw new BusinessException(ResCodeEnum.MISS_NECESSARY_PARAM, ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        FreeStorageDetail freeStorageDetail = new FreeStorageDetail();
        BeanUtils.copyProperties(freeStorageDetailDTO,freeStorageDetail);
        //查询产品名称、库存名称
        ConfigProduct configProduct = configProductMapper.selectById(freeStorageDetailDTO.getProductId());
        ConfigRepository configRepository = configRepositoryMapper.selectById(freeStorageDetailDTO.getStorageId());
        freeStorageDetail.setProductName(configProduct.getName()).setStorageName(configRepository.getName())
                .setProductUnit(configProduct.getUnitValue());
        //设置入库时间和业务id
        freeStorageDetail.setFreeStorageDetailId(UUID.randomUUID().toString().replace("-",""))
                .setStorageTime(LocalDateTime.now())
                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                .setCreateId(userInfo.getId()).setCreateTime(LocalDateTime.now()).setCreator(userInfo.getName());
        //增加自由入库明细表数据
        freeStorageDetail.insert();
        //增加仓库库存量
        StorageInventory storageInventory = new StorageInventory();
        BeanUtils.copyProperties(freeStorageDetailDTO,storageInventory);
        storageInventory.setStatus(EnumStorage.Status.normal.getKey())
                .setProductName(configProduct.getName()).setProductUnit(configProduct.getUnitValue())
                .setStorageName(configRepository.getName())
                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                .setCreateId(userInfo.getId()).setCreateTime(LocalDateTime.now()).setCreator(userInfo.getName());
        increaseInventory(storageInventory);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveFinishedProductStorage(FinishedProductPendingStorageDTO finishedProductPendingStorageDTO, AuthPlatformUserInfo userInfo) {
        if(ObjectUtils.isEmpty(finishedProductPendingStorageDTO)){
            throw new BusinessException(ResCodeEnum.MISS_NECESSARY_PARAM, ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        FinishedProductPendingStorage finishedProductPendingStorage = new FinishedProductPendingStorage();
        BeanUtils.copyProperties(finishedProductPendingStorageDTO,finishedProductPendingStorage);
        finishedProductPendingStorage.setFinishedProductPendingStorageId(UUID.randomUUID().toString().replace("-",""))
                .setStatus(EnumStorage.Status.normal.getKey())
                .setPutStorage(EnumStorage.PutStorage.not_storage.getKey())
                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                .setCreateId(userInfo.getId()).setCreator(userInfo.getName()).setCreateTime(LocalDateTime.now());
        finishedProductPendingStorage.insert();
    }

    @Override
    public ResponseResult<List<FinishedProductPendingStorageDTO>> selectAllUnStorageFinishedProduct(Pagination pagination,AuthPlatformUserInfo userInfo) {
        List<FinishedProductPendingStorage> finishedProductPendingStorages = finishedProductPendingStorageMapper.selectPage(pagination,
                new EntityWrapper<FinishedProductPendingStorage>().eq("put_storage", EnumStorage.PutStorage.not_storage.getKey())
                        .eq("factory_id",userInfo.getFactoryId()).eq("org_id",userInfo.getOrgId())
                        .eq("status", EnumStorage.Status.normal.getKey())
        );
        List<FinishedProductPendingStorageDTO> result = new ArrayList<>();
        if(!CollectionUtils.isEmpty(finishedProductPendingStorages)){
            finishedProductPendingStorages.forEach(finishedProductPendingStorage -> {
                FinishedProductPendingStorageDTO finishedProductPendingStorageDTO = new FinishedProductPendingStorageDTO();
                BeanUtils.copyProperties(finishedProductPendingStorage,finishedProductPendingStorageDTO);
                result.add(finishedProductPendingStorageDTO);
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result,PageUtils.transToPage(pagination));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveFinishedProduct(FinishedProductStorageListDTO finishedProductStorageListDTO, AuthPlatformUserInfo userInfo) {
        if(!ObjectUtils.isEmpty(finishedProductStorageListDTO)&&!CollectionUtils.isEmpty(finishedProductStorageListDTO.getFinishedProductStorageDTOS())){
            //仓库id
            Integer storageId = finishedProductStorageListDTO.getStorageId();
            //仓库名
            String storageName = finishedProductStorageListDTO.getStorageName();
            List<FinishedProductStorageDTO> finishedProductStorageDTOS = finishedProductStorageListDTO.getFinishedProductStorageDTOS();
            //copy属性，入库
            if(!CollectionUtils.isEmpty(finishedProductStorageDTOS)){
                List<StorageInventory> list = new ArrayList<>();
                for (FinishedProductStorageDTO finishedProductStorageDTO:finishedProductStorageDTOS) {
                    FinishedProductPendingStorage finishedProductPendingStorage = finishedProductPendingStorageMapper.selectOne(
                            new FinishedProductPendingStorage().setTonBagNumber(finishedProductStorageDTO.getTonBagNumber())
                                    .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                                    .setStatus(EnumStorage.Status.normal.getKey())
                    );
                    if(ObjectUtils.isEmpty(finishedProductPendingStorage)){
                        log.error("产生品批量入库时，根据吨袋编号 ：{}，在finished_product_pending_storage表中没有查到吨袋信息！ "
                                ,finishedProductStorageDTO.getTonBagNumber());
                        continue;
                    }
                    if(EnumStorage.PutStorage.storage.getKey().equals(finishedProductPendingStorage.getPutStorage())){
                        log.error("产生品批量入库时，吨袋编号 ：{}，已经入库，不能重复提交！ "
                                ,finishedProductStorageDTO.getTonBagNumber());
                        continue;
                    }
                    //更新待入库吨袋明细状态为已入库，并设置入库时间
                    finishedProductPendingStorage.setPutStorage(EnumStorage.PutStorage.storage.getKey())
                            .setStorageTime(LocalDateTime.now()).setStorageId(storageId)
                            .setModifier(userInfo.getName()).setModifyId(userInfo.getId()).setModifyTime(LocalDateTime.now())
                            .updateById();
                    //成品批量入库
                    getFinishedStorageInventory(finishedProductPendingStorage,storageId,storageName,list,userInfo);
                }
                for (StorageInventory storageInventory:list){
                    increaseInventory(storageInventory);
                }
            }

        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateFinishedProductSpecAndInsertStorage(List<ProBagStorageSpecUpdateDTO> proBagStorageSpecUpdateDTOS, AuthPlatformUserInfo userInfo) {
        if(!CollectionUtils.isEmpty(proBagStorageSpecUpdateDTOS)){
            List<StorageInventory> list = new ArrayList<>();
            for(ProBagStorageSpecUpdateDTO proBagStorageSpecUpdateDTO:proBagStorageSpecUpdateDTOS){
                String baggingCode = proBagStorageSpecUpdateDTO.getBaggingCode();
                FinishedProductPendingStorage finishedProductPendingStorage = finishedProductPendingStorageMapper.selectOne(
                        new FinishedProductPendingStorage().setTonBagNumber(baggingCode).setProductSpecId(null)
                                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                                .setStatus(EnumStorage.Status.normal.getKey())
                );
                if(!ObjectUtils.isEmpty(finishedProductPendingStorage)){
                    finishedProductPendingStorage.setProductSpecId(proBagStorageSpecUpdateDTO.getProductSpecId())
                            .setProductSpecName(proBagStorageSpecUpdateDTO.getProductSpecName())
                            .updateById();
                    Integer storageId = finishedProductPendingStorage.getStorageId();
                    //storageId不为空更新仓库
                    if(!ObjectUtils.isEmpty(storageId)){
                        String storageName = Optional.ofNullable(configRepositoryMapper.selectById(storageId))
                                .map(ConfigRepository::getName).orElse("");
                        getFinishedStorageInventory(finishedProductPendingStorage,storageId,storageName,list,userInfo);
                    }
                }
            }
            for (StorageInventory storageInventory:list){
                increaseInventory(storageInventory);
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveFinishedProductOutOfStorage(FinishedProductBeOutOfStorageDTO finishedProductBeOutOfStorageDTO, AuthPlatformUserInfo userInfo) {
        if(ObjectUtils.isEmpty(finishedProductBeOutOfStorageDTO)){
            throw new BusinessException(ResCodeEnum.MISS_NECESSARY_PARAM, ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        FinishedProductBeOutOfStorage finishedProductBeOutOfStorage = new FinishedProductBeOutOfStorage();
        BeanUtils.copyProperties(finishedProductBeOutOfStorageDTO,finishedProductBeOutOfStorage);
        //查询产品单位
        Integer productId = finishedProductBeOutOfStorageDTO.getProductId();
        ConfigProduct configProduct = configProductMapper.selectById(productId);
        if(ObjectUtils.isEmpty(configProduct)){
            throw new BusinessException(ResCodeEnum.MISS_NECESSARY_PARAM, ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        //新增产成品待出库记录
        finishedProductBeOutOfStorage.setContractCarId(UUID.randomUUID().toString().replace("-",""))
                .setStatus(EnumStorage.Status.normal.getKey()).setOutStorage(EnumStorage.OutStorageStatus.wait_storage.getKey())
                //单独设置产品名称和单位
                .setProductName(configProduct.getName()).setProductUnit(configProduct.getUnitValue())
                .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                .setCreateId(userInfo.getId()).setCreator(userInfo.getName()).setCreateTime(LocalDateTime.now());
        finishedProductBeOutOfStorage.insert();
    }


    @Override
    public ResponseResult<List<ContractCarDTO>> selectUnLoadContractCar(Pagination pagination,AuthPlatformUserInfo userInfo) {
        //查询所有未出库的车辆信息
        List<FinishedProductBeOutOfStorage> finishedProductBeOutOfStorages = finishedProductBeOutOfStorageMapper.selectPage(pagination,
                new EntityWrapper<FinishedProductBeOutOfStorage>()
                        .eq("out_storage", EnumStorage.OutStorageStatus.wait_storage.getKey())
                        .eq("factory_id",userInfo.getFactoryId()).eq("org_id",userInfo.getOrgId())
                        .eq("status", EnumStorage.Status.normal.getKey()).orderDesc(Arrays.asList("create_time"))
        );
        List<ContractCarDTO> result = new ArrayList<>();
        if(!CollectionUtils.isEmpty(finishedProductBeOutOfStorages)){
            finishedProductBeOutOfStorages.forEach(finishedProductBeOutOfStorage -> {
                //copy属性到参数类中
                ContractCarDTO contractCarDTO = new ContractCarDTO();
                BeanUtils.copyProperties(finishedProductBeOutOfStorage,contractCarDTO);
                result.add(contractCarDTO);
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result,PageUtils.transToPage(pagination));
    }

    /**
     * web查询销售待出库列表
     * @param rq 请求参数
     * @param userInfo 用户信息
     * @return 销售待出库列表
     */
    @Override
    public ResponseResult<List<ContractCarDTO>> webSearchSaleToBeOutOfStockList(SaleOutOfStockSearchRQ rq, AuthPlatformUserInfo userInfo) {

        Pagination pagination = PageUtils.transFromPage(rq.getPage());

        rq.setEnterpriseId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());

        // 查询待出库列表
        List<ContractCarDTO> dto = finishedProductBeOutOfStorageMapper.selectUnLoadContractCar(rq, pagination);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));
    }

    @Override
    public TonBagDTO selectTonBagInfo(String tonBagNumber,AuthPlatformUserInfo userInfo) {
        FinishedProductPendingStorage finishedProductPendingStorage = finishedProductPendingStorageMapper.selectOne(
                new FinishedProductPendingStorage().setTonBagNumber(tonBagNumber).setPutStorage(EnumStorage.PutStorage.storage.getKey())
                        .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                        .setStatus(EnumStorage.Status.normal.getKey())
        );
        TonBagDTO tonBagDTO = new TonBagDTO();
        if(!ObjectUtils.isEmpty(finishedProductPendingStorage)){
            tonBagDTO.setProductNumber(finishedProductPendingStorage.getProductNumber())
                    .setTonBagNumber(tonBagNumber).setProductUnit(finishedProductPendingStorage.getProductUnit());
        }
        return tonBagDTO;
    }

    /**
     *  批量产成品数量汇总
     * @param finishedProductPendingStorage 待入库的产成品实体
     * @param storageId 仓库id
     * @param storageName 仓库名称
     * @param list 入库实体
     * @param userInfo 用户信息
     */
    private void getFinishedStorageInventory(FinishedProductPendingStorage finishedProductPendingStorage,Integer storageId,String storageName
            ,List<StorageInventory> list,AuthPlatformUserInfo userInfo){
        boolean state = false;
        for(StorageInventory storageInventory:list){
            if(finishedProductPendingStorage.getProductId().equals(storageInventory.getProductId())&&
                    finishedProductPendingStorage.getProductSpecId().equals(storageInventory.getProductSpecId())){
                BigDecimal productNumber = storageInventory.getProductNumber();
                productNumber = productNumber.add(finishedProductPendingStorage.getProductNumber());
                storageInventory.setProductNumber(productNumber);
                state = true;
            }
        }
        if(!state&&!ObjectUtils.isEmpty(finishedProductPendingStorage.getProductSpecId())){
            StorageInventory storageInventory = new StorageInventory();
            storageInventory.setProductId(finishedProductPendingStorage.getProductId())
                    .setProductName(finishedProductPendingStorage.getProductName())
                    .setProductUnit(finishedProductPendingStorage.getProductUnit())
                    .setProductNumber(finishedProductPendingStorage.getProductNumber())
                    .setStorageId(storageId).setStorageName(storageName)
                    .setStatus(EnumStorage.Status.normal.getKey())
                    .setProductSpecId(finishedProductPendingStorage.getProductSpecId())
                    .setProductSpecName(finishedProductPendingStorage.getProductSpecName())
                    .setFactoryId(userInfo.getFactoryId()).setOrgId(userInfo.getOrgId())
                    .setCreateId(userInfo.getId()).setCreator(userInfo.getName()).setCreateTime(LocalDateTime.now());
            list.add(storageInventory);
        }
    }

    /**
     * 增加库存
     * @param storageInventory p
     */
    private synchronized void increaseInventory(StorageInventory storageInventory){
        if(ObjectUtils.isEmpty(storageInventory)){
            throw new BusinessException(ResCodeEnum.MISS_NECESSARY_PARAM, ExceptionMessageEnum.USER_VALUE_EMPTY);
        }
        StorageInventory oldStorageInventory = storageInventoryMapper.selectOne(new StorageInventory().setProductId(storageInventory.getProductId())
                .setStorageId(storageInventory.getStorageId()).setProductSpecId(storageInventory.getProductSpecId())
                .setStatus(EnumStorage.Status.normal.getKey()));
        if(ObjectUtils.isEmpty(oldStorageInventory)){
            //新增一条数据,后面在优化成乐观锁策略
            storageInventory.insert();
        }else {
            //更新总量,后面在优化成乐观锁策略
            BigDecimal productNumber = storageInventory.getProductNumber();
            BigDecimal oldProductNumber = oldStorageInventory.getProductNumber();
            oldStorageInventory.setProductNumber(oldProductNumber.add(productNumber));
            oldStorageInventory.setModifier(storageInventory.getCreator())
                    .setModifyId(storageInventory.getCreateId()).setModifyTime(LocalDateTime.now());
            oldStorageInventory.updateById();
        }
    }

    /**
     * 扣减库存
     * @param storageId c
     * @param productId p
     * @param productNumber p
     * @param productSpecId a
     */
    private synchronized void deductStock(Integer storageId,Integer productId,Integer productSpecId,BigDecimal productNumber,AuthPlatformUserInfo userInfo){
        //根据仓库id、产品id、规格id查询库存
        StorageInventory storageInventory = selectStorageInventory(storageId, productId, productSpecId);
        //扣减库存
        storageInventory.setProductNumber(storageInventory.getProductNumber().subtract(productNumber))
                .setModifyTime(LocalDateTime.now()).setModifier(userInfo.getName()).setModifyId(userInfo.getId())
                .updateById();
    }

    /**
     * 查询 磅单信息
     * @param machineId 磅单id
     * @return 磅单
     */
    private BuyWeightMachine getBuyWeiMachineById(String machineId) {
        //根据榜单查询货物重量
        BuyWeightMachine buyWeightMachine =
                buyWeightMachineMapper.selectOne(new BuyWeightMachine().setMachineId(machineId).setStatus(EnumStorage.Status.normal.getKey()));
        if(ObjectUtils.isEmpty(buyWeightMachine)){
            log.error("根据榜单号:{},未找到相应数据！",machineId);
            throw new BusinessException(ResCodeEnum.NO_DATA, "根据榜单号:"+machineId+" 未找到相应数据！");
        }
        return buyWeightMachine;
    }

    private <T> T getStorageEnumByCode(Class<T> clazz,String fieldMethod,Integer code) {
        T[] enumConstants = clazz.getEnumConstants();
        for (T t:enumConstants) {
            try {
                Method declaredMethod = t.getClass().getDeclaredMethod(fieldMethod);
                Object invoke = declaredMethod.invoke(t);
                if(invoke.equals(code)){
                    return t;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        throw new BusinessException(ResCodeEnum.ERROR_PARAMETER,"根据code没有查找到相应的枚举！");
    }

	@Override
	public ResponseResult<List<BuyPendingStorageListDTO>> selectBuyPendingStorageInfoByConNum(Pagination pagination,
			AuthPlatformUserInfo userInfo, String contractNum) {
		List<BuyGoodsPendingStorage> buyGoodsPendingStorages =
                buyGoodsPendingStorageMapper.selectPage(pagination,new EntityWrapper<BuyGoodsPendingStorage>()
                .eq("put_storage", EnumStorage.PutStorage.not_storage.getKey())
                .eq("factory_id",userInfo.getFactoryId()).eq("org_id",userInfo.getOrgId())
                .eq("status", EnumStorage.Status.normal.getKey())
                .like("contract_id", contractNum).orderDesc(Arrays.asList("create_time")));
        List<BuyPendingStorageListDTO> result = new ArrayList<>();
        if(!CollectionUtils.isEmpty(buyGoodsPendingStorages)){
            buyGoodsPendingStorages.forEach(buyGoodsPendingStorage -> {
            	BuyPendingStorageListDTO BuyPendingStorageListDTO = new BuyPendingStorageListDTO();
                BeanUtils.copyProperties(buyGoodsPendingStorage,BuyPendingStorageListDTO);
                
                //  当合同不存在时
                if (!StringUtils.isEmpty(buyGoodsPendingStorage.getContractId())) {
                    BuyContractBasic contractBasic = buyContractBasicMapper
                            .selectOne(new BuyContractBasic().setContractBusinessId(buyGoodsPendingStorage.getContractId())
                            		.setFactoryId(userInfo.getFactoryId())
                            		.setEnterpriseId(userInfo.getOrgId())
                                    .setStatus(Status.TRUE.getKey()));
                    if (!ObjectUtils.isEmpty(contractBasic)) {
                    	BuyPendingStorageListDTO.setSupplierName(contractBasic.getSupplierName());
                    	BuyPendingStorageListDTO.setSupplierId(contractBasic.getSupplierId());
                    }
                }
                BuyPendingStorageListDTO.setArrivalTime(
                        LocalDateUtils.formatDate(buyGoodsPendingStorage.getArrivalTime(),"yyyy年M月d日"));
                result.add(BuyPendingStorageListDTO);
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result, PageUtils.transToPage(pagination));
	}

	@Override
	public ResponseResult<List<BuyPendingStorageListDTO>> selectBuyPendingStorageInfoByConditional(
			Pagination pagination, AuthPlatformUserInfo userInfo, BuyStorageRq rq) {
		// 根据供应商名称模糊查询采购合同号
		 List<BuyContractBasic> contractBasicList = buyContractBasicMapper
                 .selectList((new EntityWrapper<BuyContractBasic>()
                		 .like("supplier_name", rq.getSupplierName())
                		 .eq("enterprise_id", userInfo.getOrgId())
                		 .eq("factory_id", userInfo.getFactoryId())
                		 .eq("status", Status.TRUE.getKey())));
		 Set<String> contractNums = contractBasicList.stream().map(a -> a.getContractNum()).collect(Collectors.toSet());
		 // 起始时间和结束时间为空
		 EntityWrapper wrapper = new EntityWrapper();
		 wrapper.eq("put_storage", EnumStorage.PutStorage.not_storage.getKey());
		 wrapper.eq("factory_id",userInfo.getFactoryId());
         wrapper.eq("org_id", userInfo.getOrgId());
         wrapper.eq("status", Status.TRUE.getKey());
         //合同号
         if(!ObjectUtils.isEmpty(rq.getContractNum())){
             wrapper.like("contract_id", rq.getContractNum());
         }
         // 供应商
         if(!ObjectUtils.isEmpty(rq.getSupplierName())){
        	 wrapper.in("contract_id", contractNums);
         }
         // 进厂时间
         if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
             wrapper.between("arrival_time",rq.getStartTime() + DateUtils.TIME_SUFFIX ,rq.getEndTime() +  DateUtils.TIME_END );
         }
         if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
             wrapper.ge("arrival_time",rq.getStartTime() + DateUtils.TIME_SUFFIX);
         }
         if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
             wrapper.le("arrival_time",rq.getEndTime() +  DateUtils.TIME_END );
         }
         wrapper.orderBy("arrival_time",false);
		// 查询入库记录
		List<BuyGoodsPendingStorage> buyGoodsPendingStorages =
                buyGoodsPendingStorageMapper.selectPage(pagination,wrapper);
		List<BuyPendingStorageListDTO> result = new ArrayList<>();
        if(!CollectionUtils.isEmpty(buyGoodsPendingStorages)){
            buyGoodsPendingStorages.forEach(buyGoodsPendingStorage -> {
            	BuyPendingStorageListDTO buyPendingStorageListDTO = new BuyPendingStorageListDTO();
                BeanUtils.copyProperties(buyGoodsPendingStorage,buyPendingStorageListDTO);

                //  当合同不存在时
                if (!StringUtils.isEmpty(buyGoodsPendingStorage.getContractId())) {
                    BuyContractBasic contractBasic = buyContractBasicMapper
                            .selectOne(new BuyContractBasic().setContractNum(buyGoodsPendingStorage.getContractId())
                            		.setFactoryId(userInfo.getFactoryId())
                            		.setEnterpriseId(userInfo.getOrgId())
                                    .setStatus(Status.TRUE.getKey()));
                    if (!ObjectUtils.isEmpty(contractBasic)) {
                    	buyPendingStorageListDTO.setSupplierName(contractBasic.getSupplierName());
                    	buyPendingStorageListDTO.setSupplierId(contractBasic.getSupplierId());
                    }
                }
                buyPendingStorageListDTO.setArrivalTime(
                        LocalDateUtils.formatDate(buyGoodsPendingStorage.getArrivalTime(),"yyyy年M月d日"));
                result.add(buyPendingStorageListDTO);
            });
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result, PageUtils.transToPage(pagination));
	}

	@Override
	public ResponseResult<BuyPendingStorageProductMsgDTO> selectBuyStorageMsg(AuthPlatformUserInfo userInfo,
			String buyProductPendingStorageId) {
		BuyPendingStorageProductMsgDTO result = new BuyPendingStorageProductMsgDTO();

		BuyGoodsPendingStorage buyGoodsPendingStorage = buyGoodsPendingStorageMapper.selectOne(new BuyGoodsPendingStorage()
                .setBuyProductPendingStorageId(buyProductPendingStorageId).setStatus(EnumStorage.Status.normal.getKey()));
        if(ObjectUtils.isEmpty(buyGoodsPendingStorage)){
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ERROR_DATA);
        }
        BeanUtils.copyProperties(buyGoodsPendingStorage,result);
        result.setArrivalTime(
                LocalDateUtils.formatDate(buyGoodsPendingStorage.getArrivalTime(),"yyyy年M月d日 HH:mm:ss"));
        result.setStorageTime(
                LocalDateUtils.formatDate(buyGoodsPendingStorage.getStorageTime(),"yyyy年M月d日 HH:mm:ss"));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
	}

	@Override
	public ResponseResult<List<BuyStorageListDTO>> selectBuyStorageInfoByConditional(Pagination pagination,
			AuthPlatformUserInfo userInfo, BuyStorageRq rq) {
		    // 根据供应商名称模糊查询采购合同号
			 List<BuyContractBasic> contractBasicList = buyContractBasicMapper
	                 .selectList((new EntityWrapper<BuyContractBasic>()
	                		 .like("supplier_name", rq.getSupplierName())
	                		 .eq("enterprise_id", userInfo.getOrgId())
	                		 .eq("factory_id", userInfo.getFactoryId())
	                		 .eq("status", Status.TRUE.getKey())));
			 Set<String> contractNums = contractBasicList.stream().map(a -> a.getContractNum()).collect(Collectors.toSet());
			// 起始时间和结束时间为空
			 EntityWrapper wrapper = new EntityWrapper();
			 wrapper.eq("put_storage", EnumStorage.PutStorage.storage.getKey());
			 wrapper.eq("factory_id",userInfo.getFactoryId());
	         wrapper.eq("org_id", userInfo.getOrgId());
	         wrapper.eq("status", Status.TRUE.getKey());
	         //合同号
	         if(!ObjectUtils.isEmpty(rq.getContractNum())){
	             wrapper.like("contract_id", rq.getContractNum());
	         }
	         // 供应商
	         if(!ObjectUtils.isEmpty(rq.getSupplierName())){
	        	 wrapper.in("contract_id", contractNums);
	         }
	         // 进厂时间
	         if (!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
	             wrapper.between("storage_time",rq.getStartTime() + DateUtils.TIME_SUFFIX ,rq.getEndTime() +  DateUtils.TIME_END );
	         }
	         if (!ObjectUtils.isEmpty(rq.getStartTime()) && ObjectUtils.isEmpty(rq.getEndTime())) {
	             wrapper.ge("storage_time",rq.getStartTime() + DateUtils.TIME_SUFFIX);
	         }
	         if (ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())) {
	             wrapper.le("storage_time",rq.getEndTime() +  DateUtils.TIME_END );
	         }
	         wrapper.orderBy("storage_time",false);

			// 查询入库记录
			List<BuyGoodsPendingStorage> buyGoodsPendingStorages =
	                buyGoodsPendingStorageMapper.selectPage(pagination,wrapper);
			List<BuyStorageListDTO> result = new ArrayList<>();
	        if(!CollectionUtils.isEmpty(buyGoodsPendingStorages)){
	            buyGoodsPendingStorages.forEach(buyGoodsPendingStorage -> {
	            	BuyStorageListDTO buyStorageListDTO = new BuyStorageListDTO();
	                BeanUtils.copyProperties(buyGoodsPendingStorage,buyStorageListDTO);

	                //  当合同不存在时
	                if (!StringUtils.isEmpty(buyGoodsPendingStorage.getContractId())) {
	                    BuyContractBasic contractBasic = buyContractBasicMapper
	                            .selectOne(new BuyContractBasic().setContractNum(buyGoodsPendingStorage.getContractId())
	                            		.setFactoryId(userInfo.getFactoryId())
	                            		.setEnterpriseId(userInfo.getOrgId())
	                                    .setStatus(Status.TRUE.getKey()));
	                    if (!ObjectUtils.isEmpty(contractBasic)) {
	                    	buyStorageListDTO.setSupplierName(contractBasic.getSupplierName());
	                    	buyStorageListDTO.setSupplierId(contractBasic.getSupplierId());
	                    }
	                }
	                buyStorageListDTO.setStorageTime(
	                        LocalDateUtils.formatDate(buyGoodsPendingStorage.getStorageTime(),"yyyy年M月d日"));
	                result.add(buyStorageListDTO);
	            });
	        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result, PageUtils.transToPage(pagination));
	}

	@Override
	public ResponseResult<List<FreeInStorageRecordDTO>> selectFreeInStorageRecord(AuthPlatformUserInfo userInfo,Pagination pagination) {
        //新增入库记录

        List<FreeInStorageRecordDTO> freeInStorageRecordDTOS = selectFreeInStorageRecordWithPage(userInfo.getFactoryId(),userInfo.getOrgId(),pagination);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,freeInStorageRecordDTOS,PageUtils.transToPage(pagination));
	}

	@Override
	public ResponseResult<List<FinishedProductPendingStorageListDTO>> selectFinishedStorageByConditional(
			Pagination pagination, AuthPlatformUserInfo userInfo, FinishedStorageRq rq) {
		HashMap<String, Object> map = new HashMap<>();
		map.put("orgId", userInfo.getOrgId());
        map.put("factoryId", userInfo.getFactoryId());
        map.put("status", Status.TRUE.getKey());
        //班次日期
        if(!ObjectUtils.isEmpty(rq.getStartTime())){
        	map.put("startTime", rq.getStartTime()+ DateUtils.TIME_SUFFIX);
        }
        if(!ObjectUtils.isEmpty(rq.getEndTime())){
        	map.put("endTime", rq.getEndTime()+ DateUtils.TIME_END);
        }
        //是否入库
        if (!ObjectUtils.isEmpty(rq.getPutStorage())) {
        	map.put("putStorage", rq.getPutStorage());
        }
        //炉号
        if (!ObjectUtils.isEmpty(rq.getFurnaceNumber())) {
        	map.put("furnaceNumber", rq.getFurnaceNumber());
        }
        //班次
        if (!ObjectUtils.isEmpty(rq.getScheduling())) {
        	map.put("scheduling", rq.getScheduling());
        }
        //吨袋编号
        if (!ObjectUtils.isEmpty(rq.getTonBagNumber())) {
        	map.put("tonBagNumber", rq.getTonBagNumber());
        }
		List<FinishedProductPendingStorageListDTO> result = finishedProductPendingStorageMapper
				.selectFinishedStorageByConditional(map, pagination);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result, PageUtils.transToPage(pagination));
	}

    /**
     * @Description 获取仓库树
     * @author chenxm66777123
     * @Date 2019/11/27 10:04
     * @version 1.0.0
     */
    @Override
    public ResponseResult<List<StorageTreeDTO>> getStorageTree(AuthPlatformUserInfo userInfo) {

        List<ConfigRepository> configRepositories = configRepositoryMapper.selectList(
                new EntityWrapper<>(new ConfigRepository().setEnterpriseId(userInfo.getOrgId())
                        .setStatus(Status.TRUE.getKey())
                        .setDeleted(Status.FALSE.getKey())));

        List<StorageTreeDTO> result = new ArrayList<>();
        EnumConfig.STORAGE_TYPE[] storageType= EnumConfig.STORAGE_TYPE.values();
        for (EnumConfig.STORAGE_TYPE item: storageType) {
            StorageTreeDTO storageTreeDTO = new StorageTreeDTO();
            List<KeyValueDTO> keyValues = new ArrayList<>();
            configRepositories.stream().forEach(obj->{
                //属于同一仓库类型
                if(obj.getType().equals(item.getKey())){
                    KeyValueDTO keyValue = new KeyValueDTO();
                    keyValue.setKey(obj.getId());
                    keyValue.setValue(obj.getName());
                    keyValues.add(keyValue);
                }
            });
            storageTreeDTO.setStorageTypeId(item.getKey());
            storageTreeDTO.setStorageTypeName(item.getValue());
            storageTreeDTO.setStorages(keyValues);
            result.add(storageTreeDTO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);

    }

    /**
     * @Description 根据仓库id获取现存量
     * @author chenxm66777123
     * @Date 2019/11/27 11:00
     * @version 1.0.0
     */
    @Override
    public ResponseResult<List<StockDTO>> getStockByStorageId(List<Integer> storageId, AuthPlatformUserInfo userInfo,Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        EntityWrapper wrapper = new EntityWrapper();
        wrapper.eq("org_id", userInfo.getOrgId());
        wrapper.eq("status", Status.TRUE.getKey());
        if(!ObjectUtils.isEmpty(storageId)){
            wrapper.in("storage_id", storageId);
        }
        List<StorageInventory> storageInventories = storageInventoryMapper.selectPage(pagination,wrapper);
        List<StockDTO> result =
                com.bee.platform.common.utils.BeanUtils.assemble(StockDTO.class,storageInventories);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result,PageUtils.transToPage(pagination));
    }

    /**
     * web 条件搜索销售已出库列表
     * @param rq 请求参数
     * @param userInfo 用户信息
     * @return 销售已出库列表
     */
    @Override
    public ResponseResult<List<SaleOutOfStockDTO>> searchSaleOutOfStockList(SaleOutOfStockSearchRQ rq, AuthPlatformUserInfo userInfo) {
        Pagination pagination = PageUtils.transFromPage(rq.getPage());
        rq.setEnterpriseId(userInfo.getOrgId()).setFactoryId(userInfo.getFactoryId());
        String startTime = rq.getStartTime();
        if (!StringUtils.isEmpty(startTime)) {
            rq.setStartTime(startTime + " 00:00:00");
        }
        String endTime = rq.getEndTime();
        if (!StringUtils.isEmpty(endTime)) {
            rq.setEndTime(endTime + " 23:59:59");
        }
        // 查询已出库列表
        List<SaleOutOfStockDTO> dto = finishedProductBeOutOfStorageMapper.searchSaleOutOfStockList(rq, pagination);
        // 遍历添加出库数量
        dto.forEach(o-> o.setNumber(getSum(o.getContractCarId())));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));

    }

    /**
     * 查询总出库数量
     * @param contractCarId  业务唯一标识id
     * @return 总出库数量
     */
    private BigDecimal getSum(String contractCarId) {
        List<FinishedProductFreeOut> one = finishedProductFreeOutMapper.selectList(new EntityWrapper<FinishedProductFreeOut>()
                .eq("contract_car_id", contractCarId)
                .eq("status", Status.TRUE.getKey()));

        List<FinishedProductOutStorageDetail> two = finishedProductOutStorageDetailMapper.selectList(new EntityWrapper<FinishedProductOutStorageDetail>()
                .eq("contract_car_id", contractCarId)
                );
        BigDecimal sumOne = one.stream().map(FinishedProductFreeOut::getProductNumber).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
        BigDecimal sumTwo = two.stream().map(FinishedProductOutStorageDetail::getProductNumber).reduce(BigDecimal::add).orElse(BigDecimal.ZERO);
        return sumOne.add(sumTwo);
    }

    /**
     * web 根据id查询销售待出库详情
     * @param id id
     * @param userInfo 用户信息
     * @return 销售待出库详情
     */
    @Override
    public ResponseResult<ContractCarDTO> getSaleToBeDeliveredById(Integer id, AuthPlatformUserInfo userInfo) {

        FinishedProductBeOutOfStorage one = finishedProductBeOutOfStorageMapper.selectById(id);

        ContractCarDTO dto = com.bee.platform.common.utils.BeanUtils.copyProperties(one, ContractCarDTO.class);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);

    }

    /**
     * web 根据id查询销售已出库详情
     * @param id id
     * @param userInfo 用户信息
     * @return 销售已出库详情
     */
    @Override
    public SaleOutOfStockDetailDTO getSaleOutOfStockById(Integer id, AuthPlatformUserInfo userInfo) {

        FinishedProductBeOutOfStorage one = finishedProductBeOutOfStorageMapper.selectById(id);
        SaleOutOfStockDetailDTO dto = com.bee.platform.common.utils.BeanUtils.copyProperties(one, SaleOutOfStockDetailDTO.class);
        if(ObjectUtils.isEmpty(one)){
            return dto;
        }
        String contractCarId = one.getContractCarId();
        if(StringUtils.isEmpty(contractCarId)){
            return dto;
        }

        // 计算总出库数量
        dto.setNumber(getSum(contractCarId));
        // 查询出库列表
        List<FinishedProductFreeOut> list = finishedProductFreeOutMapper.selectList(new EntityWrapper<FinishedProductFreeOut>()
                .eq("contract_car_id", contractCarId).eq("status", Status.TRUE.getKey()));

        List<FinishedProductFreeOutDetailDTO> oneList = com.bee.platform.common.utils.BeanUtils.assemble(FinishedProductFreeOutDetailDTO.class, list);

        // 查询吨贷出库列表
        List<SaleOutStorageTonBagDTO> twoList = finishedProductOutStorageDetailMapper.selectTonOutList(contractCarId);
        dto.setOneList(oneList).setTwoList(twoList);
        return dto;
    }

    /**
     * web 查询新增销售出库详情
     * @param userInfo 用户信息
     * @return 新增销售出库详情
     */
    @Override
    public List<PickOutStorageDetailDTO> getSaleNewOutOfStockDetails(AuthPlatformUserInfo userInfo) {
        return pickOutStorageDetailMapper.getNewOutOfStockList(userInfo.getOrgId(),userInfo.getFactoryId());
    }
}

