package com.bee.platform.cloud.si.manufacture.service.manufacturebuy.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumContract;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyContractBasicService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyLogisticsBatchService;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.AuthSupplierNameDTO;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumGenerateIdModule;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.GenerateIdService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.SupplierInfoUtils;
import com.google.common.collect.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 采购合同信息表 服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
@Service
public class BuyContractBasicServiceImpl extends ServiceImpl<BuyContractBasicMapper, BuyContractBasic> implements BuyContractBasicService {

    private final Integer ONE = 1;
    private final Integer ZERO = 0;

    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;
    @Autowired
    private BuyContractAttachmentMapper buyContractAttachmentMapper;
    @Autowired
    private GenerateIdService generateIdService;
    @Autowired
    private BuyContractPaymentMapper buyContractPaymentMapper;
    @Autowired
    private BuyContractSettlementMapper buyContractSettlementMapper;
    @Autowired
    private BuyLogisticsBatchService buyLogisticsBatchService;
    @Autowired
    private ConfigProductMapper productMapper;
    @Autowired
    private ConfigLocationMapper locationMapper;
    @Autowired
    private BuyWeightMachineMapper buyWeightMachineMapper;
    @Autowired
    private SupplierInfoUtils supplierInfoUtils;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addContractBuy(BuyContractAddRq rq, AuthPlatformUserInfo userInfo) {

        if(ObjectUtils.isEmpty(rq)){
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.SYSTEM_INVALID_PARAMS);
        }
        //合同编号确认合同唯一性
        String contractNum = rq.getContractNum();
        BuyContractBasic contractUnique = buyContractBasicMapper.selectOne(new BuyContractBasic().setContractNum(contractNum));
        if(!ObjectUtils.isEmpty(contractUnique)){
            throw new BusinessException(ResCodeEnum.CONTRACT_NUMBER_EXISTED, ExceptionMessageEnum.CONTRACT_NUMBER_EXISTED);
        }
        //采购合同基础信息新增
        BuyContractBasic basic = BeanUtils.copyProperties(rq, BuyContractBasic.class);
        String generateBusinessId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.BUY_CONTRACT.getKey());
        basic.setQuantity(ObjectUtils.isEmpty(rq.getQuantity())?BigDecimal.ZERO:rq.getQuantity());
        basic.setContractBusinessId(generateBusinessId);
        basic.setEnterpriseId(userInfo.getOrgId());
        basic.setFactoryId(userInfo.getFactoryId());
        basic.setSignDate(DateUtils.parse(rq.getSignDate().replace("Z", " UTC"), DateUtils.UTC));
        basic.setCompleted(EnumCommon.IS_COMPLETED.NO.getKey());
        basic.setPurchaserId(userInfo.getId());
        basic.setPurchaserName(userInfo.getName());
        basic.setTrafficVolume(BigDecimal.ZERO);
        basic.setArrivalVolume(BigDecimal.ZERO);
        basic.setIssuedVolume(BigDecimal.ZERO);
        basic.setUndeliveredVolume(basic.getQuantity());
        basic.setCompletedVolume(BigDecimal.ZERO);
        basic.setStatus(EnumCommon.LogicStatus.NORMAL.getKey());
        basic.setCreateId(userInfo.getId());
        basic.setCreator(userInfo.getName());
        basic.setCreateTime(new Date());
        basic.setModifyId(userInfo.getId());
        basic.setModifier(userInfo.getName());
        basic.setModifyTime(new Date());
        basic.setSettleStatus(Status.FALSE.getKey());
        //供应商
        AuthSupplierNameDTO supplierInfo = supplierInfoUtils.getSupplierInfo(rq.getSupplierId());
        basic.setSupplierId(rq.getSupplierId());
        basic.setSupplierName(supplierInfo.getName());
        //地点
        ConfigLocation location = locationMapper.selectOne(new ConfigLocation()
                .setId(rq.getAddressId())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey())
                .setDeleted(Status.FALSE.getKey()));
        if(!ObjectUtils.isEmpty(location)){
            basic.setAddressId(rq.getAddressId());
            basic.setOriginAddress(location.getName());
        }
        //产品对应单位及类别
        ConfigProduct product = productMapper.selectOne(new ConfigProduct()
                .setId(rq.getProductId())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey())
                .setDeleted(EnumCommon.LogicStatus.DELETED.getKey()));
        if(!ObjectUtils.isEmpty(product)){
            basic.setUnitValue(product.getUnitValue());
            basic.setCategoryId(product.getCategoryId());
            basic.setCategoryName(product.getCategoryName());
        }
        if(buyContractBasicMapper.insert(basic)<=0){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CONTRACT_SAVE_FAILED);
        }
        //新增附件信息
        if(!CollectionUtils.isEmpty(rq.getFiles())){
            BuyContractBasic contractAdded = buyContractBasicMapper.selectOne(new BuyContractBasic().setContractNum(contractNum));
            String businessId = contractAdded.getContractBusinessId();
            List<BuyContractAttachmentAddRq> list = rq.getFiles();
            List<BuyContractAttachment> files = BeanUtils.assemble(BuyContractAttachment.class, list);
            files.stream().forEach(file -> file
                    .setContractBusinessId(businessId)
                    .setStatus(EnumCommon.LogicStatus.NORMAL.getKey())
                    .setCreateId(userInfo.getId())
                    .setCreator(userInfo.getName())
                    .setCreateTime(new Date())
                    .setModifyId(userInfo.getId())
                    .setModifier(userInfo.getName())
                    .setModifyTime(new Date()));
            if(buyContractAttachmentMapper.insertAttachments(files)<=0){
                throw new BusinessException(ResCodeEnum.ADD_FAILED, ExceptionMessageEnum.CONTRACT_ATTACHMENT_ADD_FAILED);
            }
        }

        //创建采购合同时，默认创建一个批次
        SaveLogisticsRq saveLogisticsRq = new SaveLogisticsRq();
        saveLogisticsRq.setContractBusinessId(generateBusinessId);
        buyLogisticsBatchService.saveLogisticsSectionInfo(saveLogisticsRq, userInfo);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<BuyContractListDTO> getBuyContractList(BuyContractListRq rq, Pagination pagination, AuthPlatformUserInfo userInfo) {

        BuyContractListDTO result = new BuyContractListDTO();
        EntityWrapper<BuyContractBasic> wrapper = new EntityWrapper<>();
        if(!ObjectUtils.isEmpty(rq.getContractNum())){
            wrapper.like("contract_num",rq.getContractNum());
        }
        if(!ObjectUtils.isEmpty(rq.getProductId())){
            wrapper.eq("product_id",rq.getProductId());
        }
        if(!ObjectUtils.isEmpty(rq.getStartTime()) && !ObjectUtils.isEmpty(rq.getEndTime())){
            wrapper
                    .ge("sign_date",rq.getStartTime()+ DateUtils.TIME_SUFFIX)
                    .le("sign_date",rq.getEndTime()+ DateUtils.TIME_END);
        }
        List<BuyContractBasic> contracts = buyContractBasicMapper.selectPage(pagination,wrapper
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("completed", rq.getCompleted()).orderBy("create_time", false));
        if(!CollectionUtils.isEmpty(contracts)){
            List<BuyContractListContentDTO> contents = BeanUtils.assemble(BuyContractListContentDTO.class, contracts);
            result.setData(contents);
            result.setCount(contents.size());
        }else {
            result.setData(new ArrayList<>());
            result.setCount(ZERO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<BuyContractDetailTotalDTO> getBuyContractDetail(String contractBusinessId, AuthPlatformUserInfo userInfo) {

        BuyContractDetailTotalDTO result = new BuyContractDetailTotalDTO();

        //合同详情
        BuyContractBasic buyContract = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!ObjectUtils.isEmpty(buyContract)){
            BuyContractDetailDTO contract = BeanUtils.copyProperties(buyContract, BuyContractDetailDTO.class);
            List<BuyContractAttachment> attachments = buyContractAttachmentMapper.selectList(new EntityWrapper<BuyContractAttachment>()
                    .eq("contract_business_id", contractBusinessId)
                    .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
            if(!CollectionUtils.isEmpty(attachments)){
                List<BuyContractAttachmentAddRq> files = BeanUtils.assemble(BuyContractAttachmentAddRq.class, attachments);
                contract.setFiles(files);
            }
            result.setContract(contract);
        }else{
            result.setContract(null);
        }

        //付款情况
        List<BuyContractPayment> payments = buyContractPaymentMapper.selectList(new EntityWrapper<BuyContractPayment>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        BuyContractDetailTotalPayDTO pay = new BuyContractDetailTotalPayDTO();
        if(!CollectionUtils.isEmpty(payments)){
            List<BuyContractDetailPayDTO> buyPay = BeanUtils.assemble(BuyContractDetailPayDTO.class, payments);
            BigDecimal subTotal = buyPay.stream().filter(e -> !ObjectUtils.isEmpty(e.getPayAmount()))
                    .map(BuyContractDetailPayDTO::getPayAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
            pay.setData(buyPay);
            pay.setTotal(subTotal);
            result.setPay(pay);
        }else{
            pay.setData(null);
            pay.setTotal(BigDecimal.ZERO);
            result.setPay(pay);
        }

        //结算情况
//        Integer weightKey = EnumContract.SETTLE_STATUS.WEIGHT.getKey();
//        Integer priceKey = EnumContract.SETTLE_STATUS.PRICE.getKey();
//        Integer completedKey = EnumContract.SETTLE_STATUS.COMPLETED.getKey();
//        ArrayList<Integer> list = Lists.newArrayList(weightKey, priceKey, completedKey);
//        List<BuyContractSettlement> settlements = buyContractSettlementMapper.selectList(new EntityWrapper<BuyContractSettlement>()
//                .eq("contract_business_id", contractBusinessId)
//                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
//                .in("settlement_status", list));
//        BuyContractDetailTotalSettleDTO settle = new BuyContractDetailTotalSettleDTO();
//        if(!CollectionUtils.isEmpty(settlements)){
//            List<BuyContractDetailSettleDTO> settles = BeanUtils.assemble(BuyContractDetailSettleDTO.class, settlements);
//            BigDecimal subTotal = settles.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmountSettlement()))
//                    .map(BuyContractDetailSettleDTO::getAmountSettlement)
//                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
//            settle.setTotal(subTotal);
//            //产品单位
//            settles.forEach(e -> e.setUnitValue(buyContract.getUnitValue()));
//            settle.setData(settles);
//            result.setSettle(settle);
//        }else{
//            settle.setTotal(BigDecimal.ZERO);
//            settle.setData(null);
//            result.setSettle(settle);
//        }

        //批次情况
        ResponseResult<List<BuyContractBatchDTO>> batchInfo = buyLogisticsBatchService.getContractBatchInfo(contractBusinessId);
        if(ResCodeEnum.SUCCESS.getCode().equals(batchInfo.getCode())){
            result.setBatch(batchInfo.getObject());
        }

        //磅房备注
        List<PoundHouseDTO> remark = this.getPoundHouseRemark(contractBusinessId);
        result.setRemark(remark);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> payForBuyContract(BuyContractPayRq rq, AuthPlatformUserInfo userInfo) {

        if(ObjectUtils.isEmpty(rq)){
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.SYSTEM_INVALID_PARAMS);
        }
        //判断合同完成状态
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(rq.getContractBusinessId())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(EnumCommon.IS_COMPLETED.YES.getKey().equals(contractBasic.getCompleted())){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETED);
        }
        //查询合同已有付款 设置本次新增付款序号
        List<BuyContractPayment> paymentsExisted = buyContractPaymentMapper.selectList(new EntityWrapper<BuyContractPayment>()
                .eq("contract_business_id", rq.getContractBusinessId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        BuyContractPayment payment = BeanUtils.copyProperties(rq, BuyContractPayment.class);
        Integer serial=CollectionUtils.isEmpty(paymentsExisted)?ONE:paymentsExisted.size()+1;
        payment.setSerialNum("第"+serial+"次付款");
        //基础参数组装
        String businessId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.BUY_PAYMENT.getKey());
        payment.setContractPaymentBusinessId(businessId);
        payment.setStatus(EnumCommon.LogicStatus.NORMAL.getKey());
        payment.setPayTime(DateUtils.parse(rq.getPayTime().replace("Z", " UTC"), DateUtils.UTC));
        payment.setCreateId(userInfo.getId());
        payment.setCreator(userInfo.getName());
        payment.setCreateTime(new Date());
        payment.setModifyId(userInfo.getId());
        payment.setModifier(userInfo.getName());
        payment.setModifyTime(new Date());
        if(buyContractPaymentMapper.insert(payment)<=0){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CONTRACT_PAY_FAILED);
        }
        //修改合同总付款金额
        List<BuyContractPayment> payments = buyContractPaymentMapper.selectList(new EntityWrapper<BuyContractPayment>()
                .eq("contract_business_id", rq.getContractBusinessId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!CollectionUtils.isEmpty(payments)) {
            List<BuyContractDetailPayDTO> buyPay = BeanUtils.assemble(BuyContractDetailPayDTO.class, payments);
            BigDecimal subTotal = buyPay.stream().filter(e -> !ObjectUtils.isEmpty(e.getPayAmount()))
                    .map(BuyContractDetailPayDTO::getPayAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
            buyContractBasicMapper.updateById(contractBasic.setAmountPaymentTotal(subTotal));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateSettleAmountBuy(BuyContractSettleUpdateAmountRq rq, AuthPlatformUserInfo userInfo) {

        BuyContractSettlement settlement = buyContractSettlementMapper.selectOne(new BuyContractSettlement()
                .setContractSettlementBusinessId(rq.getContractSettlementBusinessId())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(ObjectUtils.isEmpty(settlement)){
            throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.CONTRACT_SETTLEMENT_NOT_FOUND);
        }
        //判断是否已结算
        if(EnumContract.SETTLE_STATUS.COMPLETED.getKey().equals(settlement.getSettlementStatus())){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CONTRACT_SETTLEMENT_CONFIRMED);
        }
        //判断合同完成状态
        String contractBusinessId = settlement.getContractBusinessId();
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(EnumCommon.IS_COMPLETED.YES.getKey().equals(contractBasic.getCompleted())){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETED);
        }
        //更改结算单价、金额
        settlement.setUnitPriceSettlement(rq.getUnitPriceSettlement());
        settlement.setAmountSettlement(rq.getAmountSettlement());
        settlement.setSettlementStatus(EnumContract.SETTLE_STATUS.COMPLETED.getKey());
        if(buyContractSettlementMapper.updateById(settlement)<=0){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_SETTLEMENT_AMOUNT_SAVE_FAILED);
        }
        //修改合同总结算金额
        Integer weightKey = EnumContract.SETTLE_STATUS.WEIGHT.getKey();
        Integer priceKey = EnumContract.SETTLE_STATUS.PRICE.getKey();
        Integer completedKey = EnumContract.SETTLE_STATUS.COMPLETED.getKey();
        ArrayList<Integer> list = Lists.newArrayList(weightKey, priceKey, completedKey);
        List<BuyContractSettlement> settlements = buyContractSettlementMapper.selectList(new EntityWrapper<BuyContractSettlement>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .in("settlement_status", list));
        if(!CollectionUtils.isEmpty(settlements)) {
            List<BuyContractDetailSettleDTO> settles = BeanUtils.assemble(BuyContractDetailSettleDTO.class, settlements);
            BigDecimal subTotal = settles.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmountSettlement()))
                    .map(BuyContractDetailSettleDTO::getAmountSettlement)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
            buyContractBasicMapper.updateById(contractBasic.setAmountSettlementTotal(subTotal));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> confirmSettleBuyContract(String businessId, AuthPlatformUserInfo userInfo) {

        //获取结算情况
        BuyContractSettlement settlement = buyContractSettlementMapper.selectOne(new BuyContractSettlement()
                .setContractSettlementBusinessId(businessId)
                .setSettlementStatus(EnumContract.SETTLE_STATUS.PRICE.getKey())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(ObjectUtils.isEmpty(settlement)){
            throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.CONTRACT_SETTLEMENT_NOT_FOUND_OR_CONFIRMED);
        }
        //判断合同完成状态
        String contractBusinessId = settlement.getContractBusinessId();
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(EnumCommon.IS_COMPLETED.YES.getKey().equals(contractBasic.getCompleted())){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETED);
        }
        //结算确认
        settlement.setSettlementStatus(EnumContract.SETTLE_STATUS.COMPLETED.getKey());
        if(buyContractSettlementMapper.updateById(settlement)<=0){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_SETTLEMENT_CONFIRM_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> completeBuyContract(String businessId, AuthPlatformUserInfo userInfo) {

        //判断合同完成状态
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(businessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(EnumCommon.IS_COMPLETED.YES.getKey().equals(contractBasic.getCompleted())){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETED);
        }
        //完成合同必须有付款记录，若无付款记录，则提示：付款未完成，请付款
        List<BuyContractPayment> payments = buyContractPaymentMapper.selectList(new EntityWrapper<BuyContractPayment>()
                .eq("contract_business_id", contractBasic.getContractBusinessId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(CollectionUtils.isEmpty(payments)){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_NO_PAY_RECORD);
        }
        //修改完成状态
        contractBasic.setCompleted(EnumContract.IS_COMPLETED.YES.getKey());
        contractBasic.setSettleStatus(EnumContract.IS_SETTLE.YES.getKey());
        if(buyContractBasicMapper.updateById(contractBasic)<=0){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public BuyContractListContentDTO getBuyContractByBusinessId(String businessId) {

        BuyContractListContentDTO result = new BuyContractListContentDTO();
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(businessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!ObjectUtils.isEmpty(contractBasic)){
            result = BeanUtils.copyProperties(contractBasic, BuyContractListContentDTO.class);
        }
        return result;
    }

    @Override
    public ResponseResult<List<ProductDTO>> getProducts(AuthPlatformUserInfo userInfo) {
        ArrayList<ProductDTO> result = Lists.newArrayList();
        List<ConfigProduct> products = productMapper.selectList(new EntityWrapper<ConfigProduct>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("deleted", EnumCommon.LogicStatus.DELETED.getKey()));
        if(!CollectionUtils.isEmpty(products)){
            for (ConfigProduct product:products) {
                ProductDTO dto = new ProductDTO();
                dto.setProductId(product.getId());
                dto.setProductName(product.getName());
                result.add(dto);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
    }

    @Override
    public ResponseResult<List<SupplierOrCustomersDTO>> getSuppliersOrCustomers(AuthPlatformUserInfo userInfo,Integer type) {
        Integer orgId = userInfo.getOrgId();
        Integer factoryId = userInfo.getFactoryId();
        List<SupplierOrCustomersDTO> result = Lists.newArrayList();
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
    }

    /**
     * 更新合同的在途量和到货量
     */
    @Override
    public void updateContractVolume() {

        //查询在途量不为0的完成合同
        List<BuyContractBasic> completeContracts = this.selectList(new EntityWrapper<BuyContractBasic>()
                .eq("completed", Status.TRUE.getKey()).gt("traffic_volume", 0)
                .eq("status", Status.TRUE.getKey()));
        //遍历将合同的在途量更新为0
        if (!CollectionUtils.isEmpty(completeContracts)) {
            for (BuyContractBasic contractBasic : completeContracts) {
                contractBasic.setTrafficVolume(BigDecimal.ZERO);
                this.updateById(contractBasic);
            }
        }

        /*//查询未完成合同
        List<BuyContractBasic> notCompleteContracts = this.selectList(new EntityWrapper<BuyContractBasic>()
                .eq("completed", Status.FALSE.getKey())
                .eq("status", Status.TRUE.getKey()));

        //遍历更新合同相关在途量和到货量
        if (!CollectionUtils.isEmpty(notCompleteContracts)) {
            for (BuyContractBasic contractBasic : notCompleteContracts) {
                //在途量
                BigDecimal trafficVolume = BigDecimal.ZERO;
                //到货量
                BigDecimal arrivalVolume = BigDecimal.ZERO;

                //查询合同下所有批次
                List<BuyLogisticsBatch> logisticsBatches = buyLogisticsBatchService.selectList(
                        new EntityWrapper<BuyLogisticsBatch>()
                                .eq("contract_business_id", contractBasic.getContractBusinessId())
                                .eq("status", Status.TRUE.getKey()));

                //遍历批次信息
                if (!CollectionUtils.isEmpty(logisticsBatches)) {
                    for (BuyLogisticsBatch logisticsBatch : logisticsBatches) {

                        //查询最新的阶段信息
                        List<BuyTransportSection> transportSections = buyTransportSectionService.selectList(
                                new EntityWrapper<BuyTransportSection>().eq("batch_id", logisticsBatch.getBatchId())
                                        .eq("status", Status.TRUE.getKey())
                                        .orderBy("transport_section", false));
                        if (!CollectionUtils.isEmpty(transportSections)) {
                            //最新阶段id
                            String transportSectionId = transportSections.get(0).getTransportSectionId();
                            //查询阶段下的运量总和
                            BigDecimal transportTrafficVolume = buyCarrierTransportDetailMapper.selectVolumeByTransportSection(transportSectionId);
                            if (!ObjectUtils.isEmpty(transportTrafficVolume)) {
                                trafficVolume = trafficVolume.add(transportTrafficVolume);
                            }
                        }

                    }
                }

                //查询合同到货数量
                BigDecimal contractArrivalVolume = buyWeightMachineMapper.getArrivalVolumeByContractId(contractBasic.getContractBusinessId());
                if (!ObjectUtils.isEmpty(contractArrivalVolume)) {
                    arrivalVolume = contractArrivalVolume;
                }
                trafficVolume = trafficVolume.subtract(arrivalVolume);

                contractBasic.setTrafficVolume(trafficVolume);
                contractBasic.setArrivalVolume(arrivalVolume);
                this.updateById(contractBasic);
            }
        }*/
    }

    @Override
    public List<PoundHouseDTO> getPoundHouseRemark(String contractBusinessId) {
        List<PoundHouseDTO> list = Lists.newArrayList();
        List<BuyWeightMachine> machines = buyWeightMachineMapper.selectList(new EntityWrapper<BuyWeightMachine>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!CollectionUtils.isEmpty(machines)){
            list = BeanUtils.assemble(PoundHouseDTO.class, machines);
        }
        return list;
    }

}
