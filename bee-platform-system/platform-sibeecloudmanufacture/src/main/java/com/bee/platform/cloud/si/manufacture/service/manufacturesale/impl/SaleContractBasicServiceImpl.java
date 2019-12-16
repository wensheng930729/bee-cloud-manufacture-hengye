package com.bee.platform.cloud.si.manufacture.service.manufacturesale.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumContract;
import com.bee.platform.cloud.si.manufacture.dao.mapper.*;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.*;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleContractBasicService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleLogisticsBatchService;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthCustomerNameDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumGenerateIdModule;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.GenerateIdService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.CustomerInfoUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.PageUtils;
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
 * 销售合同信息表 服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-26
 */
@Service
public class SaleContractBasicServiceImpl extends ServiceImpl<SaleContractBasicMapper, SaleContractBasic> implements SaleContractBasicService {

    private final Integer ONE = 1;
    private final Integer ZERO = 0;

    @Autowired
    private SaleContractBasicMapper saleContractBasicMapper;
    @Autowired
    private SaleContractAttachmentMapper saleContractAttachmentMapper;
    @Autowired
    private SaleContractCollectionMapper saleContractCollectionMapper;
    @Autowired
    private SaleContractSettlementMapper saleContractSettlementMapper;
    @Autowired
    private GenerateIdService generateIdService;
    @Autowired
    private SaleLogisticsBatchService saleLogisticsBatchService;
    @Autowired
    private ConfigProductMapper productMapper;
    @Autowired
    private ConfigLocationMapper locationMapper;
    @Autowired
    private CustomerInfoUtils customerInfoUtils;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addContractBuy(SaleContractAddRq rq, AuthPlatformUserInfo userInfo) {

        if(ObjectUtils.isEmpty(rq)){
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.SYSTEM_INVALID_PARAMS);
        }
        //合同编号确认合同唯一性
        String contractNum = rq.getContractNum();
        SaleContractBasic contractUnique = saleContractBasicMapper.selectOne(new SaleContractBasic().setContractNum(contractNum));
        if(!ObjectUtils.isEmpty(contractUnique)){
            throw new BusinessException(ResCodeEnum.CONTRACT_NUMBER_EXISTED, ExceptionMessageEnum.CONTRACT_NUMBER_EXISTED);
        }
        //销售合同基础信息新增
        SaleContractBasic basic = BeanUtils.copyProperties(rq, SaleContractBasic.class);
        String generateBusinessId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.SALE_CONTRACT.getKey());
        basic.setQuantity(ObjectUtils.isEmpty(rq.getQuantity())?BigDecimal.ZERO:rq.getQuantity());
        basic.setContractBusinessId(generateBusinessId);
        basic.setEnterpriseId(userInfo.getOrgId());
        basic.setFactoryId(userInfo.getFactoryId());
        basic.setSignDate(DateUtils.parse(rq.getSignDate().replace("Z", " UTC"), DateUtils.UTC));
        basic.setDeliveryDate(DateUtils.parse(rq.getDeliveryDate().replace("Z", " UTC"), DateUtils.UTC));
        basic.setCompleted(EnumCommon.IS_COMPLETED.NO.getKey());
        basic.setTrafficVolume(BigDecimal.ZERO);
        basic.setReceivedVolume(BigDecimal.ZERO);
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
        //客户
        AuthCustomerNameDTO customerInfo = customerInfoUtils.getCustomerInfo(rq.getCustomerId());
        basic.setCustomerId(rq.getCustomerId());
        basic.setCustomerName(customerInfo.getName());
        //地点
        ConfigLocation location = locationMapper.selectOne(new ConfigLocation()
                .setId(rq.getAddressId())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey())
                .setDeleted(Status.FALSE.getKey()));
        if(!ObjectUtils.isEmpty(location)){
            basic.setAddressId(rq.getAddressId());
            basic.setArrivalAddress(location.getName());
        }
        //产品对应单位
        ConfigProduct product = productMapper.selectOne(new ConfigProduct()
                .setId(rq.getProductId())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey())
                .setDeleted(EnumCommon.LogicStatus.DELETED.getKey()));
        if(!ObjectUtils.isEmpty(product)){
            basic.setUnitValue(product.getUnitValue());
            basic.setCategoryId(product.getCategoryId());
            basic.setCategoryName(product.getCategoryName());
        }
        if(saleContractBasicMapper.insert(basic)<=0){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CONTRACT_SAVE_FAILED);
        }
        //新增附件信息
        if(!CollectionUtils.isEmpty(rq.getFiles())){
            SaleContractBasic contractAdded = saleContractBasicMapper.selectOne(new SaleContractBasic().setContractNum(contractNum));
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
            if(saleContractAttachmentMapper.insertAttachments(files)<=0){
                throw new BusinessException(ResCodeEnum.ADD_FAILED, ExceptionMessageEnum.CONTRACT_ATTACHMENT_ADD_FAILED);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<SaleContractListDTO> getSaleContractList(BuyContractListRq rq, Pagination pagination, AuthPlatformUserInfo userInfo) {

        SaleContractListDTO result = new SaleContractListDTO();
        EntityWrapper<SaleContractBasic> wrapper = new EntityWrapper<>();
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
        List<SaleContractBasic> contracts = saleContractBasicMapper.selectPage(pagination,wrapper
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("completed", rq.getCompleted()).orderBy("create_time", false));
        if(!CollectionUtils.isEmpty(contracts)){
            List<SaleContractListContentDTO> contents = BeanUtils.assemble(SaleContractListContentDTO.class, contracts);
            result.setData(contents);
            result.setCount(contents.size());
        }else {
            result.setData(new ArrayList<>());
            result.setCount(ZERO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<SaleContractDetailTotalDTO> getSaleContractDetail(String contractBusinessId, AuthPlatformUserInfo userInfo) {

        SaleContractDetailTotalDTO result = new SaleContractDetailTotalDTO();

        //合同详情
        SaleContractBasic saleContract = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(contractBusinessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!ObjectUtils.isEmpty(saleContract)){
            SaleContractDetailDTO contract = BeanUtils.copyProperties(saleContract, SaleContractDetailDTO.class);
            List<SaleContractAttachment> attachments = saleContractAttachmentMapper.selectList(new EntityWrapper<SaleContractAttachment>()
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

        //收款情况
        List<SaleContractCollection> collections = saleContractCollectionMapper.selectList(new EntityWrapper<SaleContractCollection>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        SaleContractDetailTotalReceiveDTO receive = new SaleContractDetailTotalReceiveDTO();
        if(!CollectionUtils.isEmpty(collections)){
            List<SaleContractDetailReceiveDTO> saleReceive = BeanUtils.assemble(SaleContractDetailReceiveDTO.class, collections);
            BigDecimal subTotal = saleReceive.stream().filter(e -> !ObjectUtils.isEmpty(e.getPaymentAmount()))
                    .map(SaleContractDetailReceiveDTO::getPaymentAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
            receive.setData(saleReceive);
            receive.setTotal(subTotal);
            result.setReceive(receive);
        }else{
            receive.setData(null);
            receive.setTotal(BigDecimal.ZERO);
            result.setReceive(receive);
        }

        //结算情况
        Integer weightKey = EnumContract.SETTLE_STATUS.WEIGHT.getKey();
        Integer priceKey = EnumContract.SETTLE_STATUS.PRICE.getKey();
        Integer completedKey = EnumContract.SETTLE_STATUS.COMPLETED.getKey();
        ArrayList<Integer> list = Lists.newArrayList(weightKey, priceKey, completedKey);
        List<SaleContractSettlement> settlements = saleContractSettlementMapper.selectList(new EntityWrapper<SaleContractSettlement>()
                .eq("contract_business_id", contractBusinessId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .in("settlement_status", list));
        BuyContractDetailTotalSettleDTO settle = new BuyContractDetailTotalSettleDTO();
        if(!CollectionUtils.isEmpty(settlements)){
            List<BuyContractDetailSettleDTO> settles = BeanUtils.assemble(BuyContractDetailSettleDTO.class, settlements);
            BigDecimal subTotal = settles.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmountSettlement()))
                    .map(BuyContractDetailSettleDTO::getAmountSettlement)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
            settle.setData(settles);
            settle.setTotal(subTotal);
            result.setSettle(settle);
        }else{
            settle.setData(null);
            settle.setTotal(BigDecimal.ZERO);
            result.setSettle(settle);
        }

        //批次情况
        ResponseResult<List<SaleContractBatchDTO>> batchInfo = saleLogisticsBatchService.getContractBatchInfo(contractBusinessId);
        if(ResCodeEnum.SUCCESS.getCode().equals(batchInfo.getCode())){
            result.setBatch(batchInfo.getObject());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> receiveForSaleContract(SaleContractReceiveRq rq, AuthPlatformUserInfo userInfo) {

        if(ObjectUtils.isEmpty(rq)){
            throw new BusinessException(ResCodeEnum.ERROR_PARAMETER, ExceptionMessageEnum.SYSTEM_INVALID_PARAMS);
        }
        //判断合同完成状态
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(rq.getContractBusinessId())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(EnumCommon.IS_COMPLETED.YES.getKey().equals(contractBasic.getCompleted())){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETED);
        }
        //查询合同已有付款 设置本次新增付款序号
        List<SaleContractCollection> collectionsExisted = saleContractCollectionMapper.selectList(new EntityWrapper<SaleContractCollection>()
                .eq("contract_business_id", rq.getContractBusinessId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        SaleContractCollection collection = BeanUtils.copyProperties(rq, SaleContractCollection.class);
        Integer serial=CollectionUtils.isEmpty(collectionsExisted)?ONE:collectionsExisted.size()+1;
        collection.setSerialNum("第"+serial+"次收款");
        //基础参数组装
        String businessId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.SALE_COLLECTION.getKey());
        collection.setContractCollectionBusinessId(businessId);
        collection.setStatus(EnumCommon.LogicStatus.NORMAL.getKey());
        collection.setReceiveTime(DateUtils.parse(rq.getReceiveTime().replace("Z", " UTC"), DateUtils.UTC));
        collection.setCreateId(userInfo.getId());
        collection.setCreator(userInfo.getName());
        collection.setCreateTime(new Date());
        collection.setModifyId(userInfo.getId());
        collection.setModifier(userInfo.getName());
        collection.setModifyTime(new Date());
        if(saleContractCollectionMapper.insert(collection)<=0){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CONTRACT_PAY_FAILED);
        }
        //修改合同总收款金额
        List<SaleContractCollection> collections = saleContractCollectionMapper.selectList(new EntityWrapper<SaleContractCollection>()
                .eq("contract_business_id", rq.getContractBusinessId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!CollectionUtils.isEmpty(collections)) {
            List<SaleContractDetailReceiveDTO> saleReceive = BeanUtils.assemble(SaleContractDetailReceiveDTO.class, collections);
            BigDecimal subTotal = saleReceive.stream().filter(e -> !ObjectUtils.isEmpty(e.getPaymentAmount()))
                    .map(SaleContractDetailReceiveDTO::getPaymentAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
            saleContractBasicMapper.updateById(contractBasic.setAmountCollectionTotal(subTotal));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateSettleAmountSaleContract(BuyContractSettleUpdateAmountRq rq, AuthPlatformUserInfo userInfo) {

        SaleContractSettlement settlement = saleContractSettlementMapper.selectOne(new SaleContractSettlement()
                .setContractSettlementBusinessId(rq.getContractSettlementBusinessId())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(ObjectUtils.isEmpty(settlement)){
            throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.CONTRACT_SETTLEMENT_NOT_FOUND);
        }
        //判断是否已结算
        if(EnumContract.SETTLE_STATUS.PRICE.getKey().equals(settlement.getSettlementStatus()) ||
                EnumContract.SETTLE_STATUS.COMPLETED.getKey().equals(settlement.getSettlementStatus())){
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CONTRACT_SETTLEMENT_CONFIRMED);
        }
        //判断合同完成状态
        String contractBusinessId = settlement.getContractBusinessId();
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(contractBusinessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(EnumCommon.IS_COMPLETED.YES.getKey().equals(contractBasic.getCompleted())){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETED);
        }
        //更改结算单价、金额
        settlement.setUnitPriceSettlement(rq.getUnitPriceSettlement());
        settlement.setAmountSettlement(rq.getAmountSettlement());
        if(saleContractSettlementMapper.updateById(settlement)<=0){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_SETTLEMENT_AMOUNT_SAVE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> confirmSettleSaleContract(String businessId, AuthPlatformUserInfo userInfo) {

        //获取结算情况
        SaleContractSettlement settlement = saleContractSettlementMapper.selectOne(new SaleContractSettlement()
                .setContractSettlementBusinessId(businessId)
                .setSettlementStatus(EnumContract.SETTLE_STATUS.PRICE.getKey())
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(ObjectUtils.isEmpty(settlement)){
            throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.CONTRACT_SETTLEMENT_NOT_FOUND_OR_CONFIRMED);
        }
        //判断合同完成状态
        String contractBusinessId = settlement.getContractBusinessId();
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(contractBusinessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(EnumCommon.IS_COMPLETED.YES.getKey().equals(contractBasic.getCompleted())){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETED);
        }
        //结算确认
        settlement.setSettlementStatus(EnumContract.SETTLE_STATUS.COMPLETED.getKey());
        if(saleContractSettlementMapper.updateById(settlement)<=0){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_SETTLEMENT_CONFIRM_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> completeSaleContract(SaleContractCompleteRq rq, AuthPlatformUserInfo userInfo) {

        String businessId = rq.getBusinessId();
        //判断合同完成状态
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(businessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(EnumCommon.IS_COMPLETED.YES.getKey().equals(contractBasic.getCompleted())){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETED);
        }
        //批次信息未完善或无批次信息，无法完成合同
        ResponseResult<List<SaleContractBatchDTO>> batchInfo = saleLogisticsBatchService.getContractBatchInfo(businessId);
        if(!ResCodeEnum.SUCCESS.getCode().equals(batchInfo.getCode()) ||
                (ResCodeEnum.SUCCESS.getCode().equals(batchInfo.getCode()) && ObjectUtils.isEmpty(batchInfo.getObject()))){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_NO_BATCH);
        }
        //修改完成状态
        contractBasic.setCompleted(EnumContract.IS_COMPLETED.YES.getKey());
        contractBasic.setSettleStatus(EnumContract.IS_SETTLE.YES.getKey());
        if(saleContractBasicMapper.updateById(contractBasic)<=0){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public SaleContractListContentDTO getSaleContractByBusinessId(String businessId) {

        SaleContractListContentDTO result = new SaleContractListContentDTO();
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(new SaleContractBasic()
                .setContractBusinessId(businessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(!ObjectUtils.isEmpty(contractBasic)){
            result = BeanUtils.copyProperties(contractBasic, SaleContractListContentDTO.class);
        }
        return result;
    }

    /**
     * 更新合同的在途量和到货量
     */
    @Override
    public void updateContractVolume() {

        //查询在途量不为0的完成合同
        List<SaleContractBasic> completeContracts = this.selectList(new EntityWrapper<SaleContractBasic>()
                .eq("completed", Status.TRUE.getKey()).gt("traffic_volume", 0)
                .eq("status", Status.TRUE.getKey()));
        //遍历将合同的在途量更新为0
        if (!CollectionUtils.isEmpty(completeContracts)) {
            for (SaleContractBasic contractBasic : completeContracts) {
                contractBasic.setTrafficVolume(BigDecimal.ZERO);
                this.updateById(contractBasic);
            }
        }

        /*//查询未完成合同
        List<SaleContractBasic> notCompleteContracts = this.selectList(new EntityWrapper<SaleContractBasic>()
                .eq("completed", Status.FALSE.getKey())
                .eq("status", Status.TRUE.getKey()));

        //遍历更新合同相关在途量和到货量
        if (!CollectionUtils.isEmpty(notCompleteContracts)) {
            for (SaleContractBasic contractBasic : notCompleteContracts) {
                //在途量
                BigDecimal trafficVolume;

                //查询合同下称重重量
                BigDecimal receivedVolume = saleWeightMachineMapper.getTotalByContract(contractBasic.getContractBusinessId());

                //查询合同到货数量
                BigDecimal contractReceivedVolume = saleCarrierTransportDetailMapper.selectVolumeByContract(contractBasic.getContractBusinessId());
                if (!ObjectUtils.isEmpty(contractReceivedVolume)) {
                    trafficVolume = receivedVolume.subtract(contractReceivedVolume);
                } else {
                    trafficVolume = receivedVolume;
                }

                contractBasic.setTrafficVolume(trafficVolume);
                contractBasic.setReceivedVolume(receivedVolume);
                this.updateById(contractBasic);
            }
        }*/
    }

}
