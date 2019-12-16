package com.bee.platform.cloud.si.manufacture.service.manufacturebuy.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumContract;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BuyContractBasicMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BuyContractSettlementMapper;
import com.bee.platform.cloud.si.manufacture.dao.mapper.BuyWeightMachineMapper;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.BuyContractBasic;
import com.bee.platform.cloud.si.manufacture.entity.BuyContractSettlement;
import com.bee.platform.cloud.si.manufacture.entity.BuyWeightMachine;
import com.bee.platform.cloud.si.manufacture.enums.EnumSampleRelation;
import com.bee.platform.cloud.si.manufacture.rq.BuyContractSettlementRQ;
import com.bee.platform.cloud.si.manufacture.rq.BuySettleListRq;
import com.bee.platform.cloud.si.manufacture.rq.BuySettlePopupWindowSaveRq;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyContractBasicService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyContractSettlementService;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
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
import com.bee.platform.common.utils.StringUtil;
import com.google.common.collect.Lists;
import com.google.gson.JsonObject;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import springfox.documentation.spring.web.json.Json;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
@Slf4j
@Service
public class BuyContractSettlementServiceImpl extends ServiceImpl<BuyContractSettlementMapper, BuyContractSettlement> implements BuyContractSettlementService {

    @Autowired
    private BuyContractBasicMapper buyContractBasicMapper;

    @Autowired
    private BuyContractSettlementMapper buyContractSettlementMapper;

    @Autowired
    private GenerateIdService generateIdService;

    @Autowired
    private BuyContractBasicService buyContractBasicService;

    @Autowired
    private BuyWeightMachineMapper buyWeightMachineMapper;

    private final Integer ZERO = 0;

    @Override
    public ResponseResult<BuyContractListDTO> getSettleListBuyContract(Integer settleStatus, AuthPlatformUserInfo userInfo, Pagination pagination) {

        BuyContractListDTO result = new BuyContractListDTO();
        List<BuyContractBasic> contracts = buyContractBasicMapper.selectPage(pagination, new EntityWrapper<BuyContractBasic>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("settle_status", settleStatus)
                .orderBy("create_time", false));
        if (!CollectionUtils.isEmpty(contracts)) {
            List<BuyContractListContentDTO> contents = new ArrayList<>();
            for (BuyContractBasic contract : contracts) {
                BuyContractListContentDTO contentDTO = BeanUtils.copyProperties(contract, BuyContractListContentDTO.class);
                //如果是已结算合同，查询总结算数量
                if (EnumContract.IS_SETTLE.YES.getKey().equals(contract.getSettleStatus())) {
                    contentDTO.setSettlementVolume(buyContractSettlementMapper.getSettlementVolume(contract.getContractBusinessId()));
                }
                contents.add(contentDTO);
            }
            result.setCount(contents.size());
            result.setData(contents);
        } else {
            result.setCount(ZERO);
            result.setData(new ArrayList<>());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result, PageUtils.transToPage(pagination));

    }

    /**
     * 根据合同业务id查询合同结算详情
     * @param contractBusinessId
     * @return
     */
    @Override
    public ResponseResult<BuyContractSettleInfoDTO> getContractSettleInfo(String contractBusinessId) {

        BuyContractSettleInfoDTO contractSettleInfoDTO = new BuyContractSettleInfoDTO();

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询合同结算详情失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        //合同信息
        BuyContractListContentDTO contractInfoDTO = BeanUtils.copyProperties(contractBasic, BuyContractListContentDTO.class);
        contractInfoDTO.setLinkMan(contractBasic.getLinkMan() + " " + contractBasic.getLinkPhone());
        contractSettleInfoDTO.setContractInfoDTO(contractInfoDTO);
        contractSettleInfoDTO.setSettleStatus(contractBasic.getSettleStatus());

        //查询合同结算情况
        List<BuyContractSettlement> contractSettlements = this.selectList(new EntityWrapper<BuyContractSettlement>()
                .eq("contract_business_id", contractBusinessId).eq("status", Status.TRUE.getKey())
                .orderBy("create_time", true));
        if (!CollectionUtils.isEmpty(contractSettlements)) {
            List<BuyContractDetailSettleDTO> settleDTOS = BeanUtils.assemble(BuyContractDetailSettleDTO.class, contractSettlements);
            contractSettleInfoDTO.setSettleDTOS(settleDTOS);
        }

        //磅房备注
        List<PoundHouseDTO> remark = buyContractBasicService.getPoundHouseRemark(contractBusinessId);
        contractSettleInfoDTO.setRemark(remark);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, contractSettleInfoDTO);
    }

    /**
     * 保存合同结算情况
     * @param contractSettlementRQ
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> saveContractSettleInfo(BuyContractSettlementRQ contractSettlementRQ, AuthPlatformUserInfo userInfo) {

        BuyContractSettlement contractSettlement = BeanUtils.copyProperties(contractSettlementRQ, BuyContractSettlement.class);
        contractSettlement.setSettleTime(DateUtils.parse(contractSettlementRQ.getSettleTime(),DateUtils.Y_M_D));
        //根据合同业务id查询当前结算数量
        Integer settleCount = this.selectCount(new EntityWrapper<BuyContractSettlement>()
                .eq("contract_business_id", contractSettlementRQ.getContractBusinessId())
                .eq("status", Status.TRUE.getKey()));

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractSettlementRQ.getContractBusinessId()).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息，查询合同结算详情失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }

        //结算序号
        int serialNum = ObjectUtils.isEmpty(settleCount) ? 1 : settleCount+1;
        //数字转小写中文
        contractSettlement.setSerialNum("第" + StringUtil.changeNum(serialNum) + "次结算");
        //合同结算表业务id
        contractSettlement.setContractSettlementBusinessId(generateIdService.generateBusinessId(EnumGenerateIdModule.Module.BUY_SETTLE.getKey()));
        //结算状态:重量确认
        contractSettlement.setSettlementStatus(EnumContract.SETTLE_STATUS.PRICE.getKey());

        //更新合同的完成数量(结算数量)
        contractBasic.setCompletedVolume(contractBasic.getCompletedVolume().add(contractSettlement.getWeightSettle()));
        buyContractBasicMapper.updateById(contractBasic);

        contractSettlement.setStatus(Status.TRUE.getKey());
        contractSettlement.setCreateId(userInfo.getId());
        contractSettlement.setCreator(userInfo.getName());
        contractSettlement.setCreateTime(new Date());
        if (this.insert(contractSettlement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
    }

    /**
     * 确认合同重量结算
     * @param contractSettlementBusinessId
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> sureSettleWeight(String contractSettlementBusinessId, AuthPlatformUserInfo userInfo) {

        //查询结算信息
        BuyContractSettlement contractSettlement = this.selectOne(new EntityWrapper<BuyContractSettlement>()
                .eq("contract_settlement_business_id", contractSettlementBusinessId)
                .eq("status", Status.TRUE.getKey())
                .eq("settlement_status", EnumContract.SETTLE_STATUS.WEIGHT.getKey()));
        if (ObjectUtils.isEmpty(contractSettlement) || ObjectUtils.isEmpty(contractSettlement.getId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        contractSettlement.setSettlementStatus(EnumContract.SETTLE_STATUS.PRICE.getKey());
        contractSettlement.setModifyId(userInfo.getId());
        contractSettlement.setModifier(userInfo.getName());
        contractSettlement.setModifyTime(new Date());
        if (this.updateById(contractSettlement)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

    }

    /**
     * 合同确认结算
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    @Override
    public ResponseResult<ResCodeEnum> sureContractSettle(String contractBusinessId, AuthPlatformUserInfo userInfo) {

        //查询合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId).setStatus(Status.TRUE.getKey()));
        if (ObjectUtils.isEmpty(contractBasic) || ObjectUtils.isEmpty(contractBasic.getId())) {
            log.error("未查询到采购合同信息！");
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        //合同改为已结算
        contractBasic.setSettleStatus(EnumContract.IS_SETTLE.YES.getKey());
        contractBasic.setModifyId(userInfo.getId());
        contractBasic.setModifier(userInfo.getName());
        contractBasic.setModifyTime(new Date());
        if (buyContractBasicMapper.updateById(contractBasic) <= 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<List<BuyContractSettleListDTO>> getSettleListBuy(AuthPlatformUserInfo userInfo, BuySettleListRq rq,Pagination pagination) {
        List<BuyContractSettleListDTO> result;
        //检索参数
        Map<String, Object> param = new HashMap<>(10);
        param.put("enterpriseId", userInfo.getOrgId());
        param.put("factoryId", userInfo.getFactoryId());
        param.put("settleStatus", rq.getSettleStatus());
        if (!ObjectUtils.isEmpty(rq.getContractNum())) {
            param.put("contractNum", rq.getContractNum());
        }
        if (!ObjectUtils.isEmpty(rq.getSupplierName())) {
            param.put("supplierName", rq.getSupplierName());
        }
        if(!ObjectUtils.isEmpty(rq.getProductName())){
            param.put("productName", rq.getProductName());
        }
        if(!ObjectUtils.isEmpty(rq.getSignDateStart())){
            param.put("signDateStart", rq.getSignDateStart()+ DateUtils.TIME_SUFFIX);
        }
        if(!ObjectUtils.isEmpty(rq.getSignDateEnd())){
            param.put("signDateEnd", rq.getSignDateEnd()+ DateUtils.TIME_END);
        }
        if(!ObjectUtils.isEmpty(rq.getSettleTimeStart())){
            param.put("settleTimeStart", rq.getSettleTimeStart()+ DateUtils.TIME_SUFFIX);
        }
        if(!ObjectUtils.isEmpty(rq.getSettleTimeEnd())){
            param.put("settleTimeEnd", rq.getSettleTimeEnd()+ DateUtils.TIME_END);
        }
        //执行查询
        result = buyContractBasicMapper.getSettleListBuy(param, pagination);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<BuyContractSettlePopupDTO> getSettleBuyPopupWindow(AuthPlatformUserInfo userInfo,
                                                                             String contractBusinessId,Integer settleStatus,Integer settleId) {
        BuyContractSettlePopupDTO result;
        //合同信息
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setEnterpriseId(userInfo.getOrgId())
                .setFactoryId(userInfo.getFactoryId())
                .setContractBusinessId(contractBusinessId)
                .setStatus(Status.TRUE.getKey()));
        if(ObjectUtils.isEmpty(contractBasic)){
            throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.CONTRACT_BUSINESS_ID_CAN_FIND_DATA);
        }
        result = BeanUtils.copyProperties(contractBasic, BuyContractSettlePopupDTO.class);
        //结算信息
        List<BuyContractSettlement> settlements = buyContractSettlementMapper.selectList(new EntityWrapper<BuyContractSettlement>()
                .eq("id", settleId)
                .eq("contract_business_id", contractBusinessId)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        BigDecimal weightSettle = BigDecimal.ZERO;
        List<BuyWeightMachine> machines = null;
        List<BuyContractSettlePopupCarDTO> settles = Lists.newArrayList();
        if(!CollectionUtils.isEmpty(settlements)) {
            //结算重量 结算单价 结算总价
            weightSettle = settlements.stream().filter(e -> !ObjectUtils.isEmpty(e.getWeightSettle()))
                    .map(BuyContractSettlement::getWeightSettle)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
            result.setWeightSettle(weightSettle);
            BigDecimal unitPrice = settlements.get(0).getUnitPriceSettlement();
            result.setUnitPriceSettlement(unitPrice);
            result.setAmountSettlement(weightSettle.multiply(unitPrice).setScale(2, BigDecimal.ROUND_HALF_UP));
            //结算相关信息
            String weightIds = settlements.get(0).getWeightIds();
            List<String> ids = JSONObject.parseArray(weightIds, String.class);
            //结算信息
            EntityWrapper<BuyWeightMachine> wrapper = new EntityWrapper<>();
            if(!CollectionUtils.isEmpty(ids)){
                wrapper.in("id", ids);
                machines = buyWeightMachineMapper.selectList(wrapper.eq("enterprise_id", userInfo.getOrgId())
                        .eq("factory_id", userInfo.getFactoryId())
                        .eq("contract_business_id", contractBusinessId)
                        .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                        .eq("settle_status", settleStatus)
                        .andNew()
                        .eq("assay_result", Status.TRUE.getKey()).or().eq("in_storage_confirm", Status.TRUE.getKey()));
                if(!CollectionUtils.isEmpty(machines)){
                    settles = BeanUtils.assemble(BuyContractSettlePopupCarDTO.class, machines);
                }
                result.setSettles(settles);
            }
        }else{
            //结算信息
            EntityWrapper<BuyWeightMachine> wrapper = new EntityWrapper<>();
            machines = buyWeightMachineMapper.selectList(wrapper.eq("enterprise_id", userInfo.getOrgId())
                    .eq("factory_id", userInfo.getFactoryId())
                    .eq("contract_business_id", contractBusinessId)
                    .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                    .eq("settle_status", settleStatus)
                    .andNew()
                    .eq("assay_result", Status.TRUE.getKey()).or().eq("in_storage_confirm", Status.TRUE.getKey()));
            if(!CollectionUtils.isEmpty(machines)){
                settles = BeanUtils.assemble(BuyContractSettlePopupCarDTO.class, machines);
            }
            result.setSettles(settles);
        }
        if(Status.TRUE.getKey().equals(settleStatus)){
            //总净重
            if(!CollectionUtils.isEmpty(machines)){
                BigDecimal weightNet = settles.stream().filter(e -> !ObjectUtils.isEmpty(e.getNetWeight()))
                        .map(BuyContractSettlePopupCarDTO::getNetWeight)
                        .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
                result.setWeightNet(weightNet);
            }
            if(!CollectionUtils.isEmpty(settlements)){
                //扣重
                result.setWeightDeduct(result.getWeightNet().subtract(weightSettle).setScale(2, BigDecimal.ROUND_HALF_UP));
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveSettleBuyPopupWindow(AuthPlatformUserInfo userInfo, BuySettlePopupWindowSaveRq rq) {
        //判断合同完成状态
        String contractBusinessId = rq.getContractBusinessId();
        BuyContractBasic contractBasic = buyContractBasicMapper.selectOne(new BuyContractBasic()
                .setContractBusinessId(contractBusinessId)
                .setStatus(EnumCommon.LogicStatus.NORMAL.getKey()));
        if(EnumCommon.IS_COMPLETED.YES.getKey().equals(contractBasic.getCompleted())){
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.CONTRACT_COMPLETED);
        }
        List<Long> ids = rq.getSettles();
        //结算参数
        BuyContractSettlement settlement = new BuyContractSettlement();
        String businessId = generateIdService.generateBusinessId(EnumGenerateIdModule.Module.BUY_SETTLE.getKey());
        settlement.setContractSettlementBusinessId(businessId);
        settlement.setContractBusinessId(rq.getContractBusinessId());
        settlement.setUnitPriceSettlement(rq.getUnitPriceSettlement());
        settlement.setWeightSettle(rq.getWeightSettle());
        settlement.setSettlementStatus(EnumContract.SETTLE_STATUS.COMPLETED.getKey());
        settlement.setStatus(EnumCommon.LogicStatus.NORMAL.getKey());
        settlement.setCreateId(userInfo.getId());
        settlement.setModifyId(userInfo.getId());
        settlement.setCreator(userInfo.getName());
        settlement.setModifier(userInfo.getName());
        settlement.setSettleTime(new Date());
        settlement.setCreateTime(new Date());
        settlement.setModifyTime(new Date());
        //车辆数
        settlement.setCarNum(CollectionUtils.isEmpty(ids)?ZERO:ids.size());
        settlement.setWeightIds(JSONObject.toJSONString(ids));
        buyContractSettlementMapper.insert(settlement);
        //磅单确认
        List<BuyWeightMachine> machines = buyWeightMachineMapper.selectList(new EntityWrapper<BuyWeightMachine>()
                .eq("enterprise_id", userInfo.getOrgId())
                .eq("factory_id", userInfo.getFactoryId())
                .in("id", ids)
                .eq("contract_business_id", rq.getContractBusinessId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        machines.stream().forEach(e ->buyWeightMachineMapper.updateById(e.setSettleStatus(Status.TRUE.getKey()).setSettleTime(new Date())));
        //修改合同总结算金额\合同结算状态\合同完成金额
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
            //结算金额
            BigDecimal amountSettle = settles.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmountSettlement()))
                    .map(BuyContractDetailSettleDTO::getAmountSettlement)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
            //合同结算金额
            contractBasic.setAmountSettlementTotal(amountSettle);
            //合同结算状态
            contractBasic.setSettleStatus(EnumContract.IS_SETTLE.YES.getKey());
            //结算数量
            BigDecimal weightSettle = settles.stream().filter(e -> !ObjectUtils.isEmpty(e.getWeightSettle()))
                    .map(BuyContractDetailSettleDTO::getWeightSettle)
                    .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
            //合同完成数量(结算数量)
            BigDecimal completedVolume = contractBasic.getCompletedVolume();
            contractBasic.setCompletedVolume(ObjectUtils.isEmpty(completedVolume)?BigDecimal.ZERO:completedVolume.add(weightSettle));
            buyContractBasicMapper.updateById(contractBasic);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

}
