package com.bee.platform.cloud.si.manufacture.service.manufacturesale.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumContract;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleContractBasicMapper;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractDetailSettleDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractDetailTotalSettleDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleContractBasic;
import com.bee.platform.cloud.si.manufacture.entity.SaleContractSettlement;
import com.bee.platform.cloud.si.manufacture.dao.mapper.SaleContractSettlementMapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.rq.SaleContractSettlementRQ;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleContractSettlementService;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumGenerateIdModule;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.GenerateIdService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.DateUtils;
import com.bee.platform.common.utils.StringUtil;
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
 * 销售合同结算表 服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-26
 */
@Service
public class SaleContractSettlementServiceImpl extends ServiceImpl<SaleContractSettlementMapper, SaleContractSettlement> implements SaleContractSettlementService {

    @Autowired
    private SaleContractBasicMapper saleContractBasicMapper;
    @Autowired
    private SaleContractSettlementMapper saleContractSettlementMapper;
    @Autowired
    private GenerateIdService generateIdService;

    /**
     * 保存合同结算情况
     * @param contractSettlementRQ
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> saveContractSettleInfo(SaleContractSettlementRQ contractSettlementRQ, AuthPlatformUserInfo userInfo) {

        SaleContractSettlement contractSettlement = BeanUtils.copyProperties(contractSettlementRQ, SaleContractSettlement.class);
        contractSettlement.setSettleTime(DateUtils.parse(contractSettlementRQ.getSettleTime(),DateUtils.Y_M_D));
        //根据合同业务id查询当前结算数量
        Integer settleCount = this.selectCount(new EntityWrapper<SaleContractSettlement>()
                .eq("contract_business_id", contractSettlementRQ.getContractBusinessId())
                .eq("status", Status.TRUE.getKey()));
        //结算序号
        int serialNum = ObjectUtils.isEmpty(settleCount) ? 1 : settleCount+1;
        //数字转小写中文
        contractSettlement.setSerialNum("第" + StringUtil.changeNum(serialNum) + "次结算");
        //合同结算表业务id
        contractSettlement.setContractSettlementBusinessId(generateIdService.generateBusinessId(EnumGenerateIdModule.Module.SALE_SETTLE.getKey()));
        //结算状态:重量确认
        contractSettlement.setSettlementStatus(EnumContract.SETTLE_STATUS.COMPLETED.getKey());

        //更新合同的完成数量(结算数量)
        SaleContractBasic contractBasic = saleContractBasicMapper.selectOne(
                new SaleContractBasic().setContractBusinessId(contractSettlementRQ.getContractBusinessId()));
        contractBasic.setCompletedVolume(contractBasic.getCompletedVolume().add(contractSettlement.getWeightSettle()));
        saleContractBasicMapper.updateById(contractBasic);

        contractSettlement.setStatus(Status.TRUE.getKey());
        contractSettlement.setCreateId(userInfo.getId());
        contractSettlement.setCreator(userInfo.getName());
        contractSettlement.setCreateTime(new Date());
        if (this.insert(contractSettlement)) {
            //修改合同总结算金额
            Integer weightKey = EnumContract.SETTLE_STATUS.WEIGHT.getKey();
            Integer priceKey = EnumContract.SETTLE_STATUS.PRICE.getKey();
            Integer completedKey = EnumContract.SETTLE_STATUS.COMPLETED.getKey();
            ArrayList<Integer> list = Lists.newArrayList(weightKey, priceKey, completedKey);
            List<SaleContractSettlement> settlements = saleContractSettlementMapper.selectList(new EntityWrapper<SaleContractSettlement>()
                    .eq("contract_business_id", contractSettlementRQ.getContractBusinessId())
                    .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                    .in("settlement_status", list));
            if(!CollectionUtils.isEmpty(settlements)) {
                List<BuyContractDetailSettleDTO> settles = BeanUtils.assemble(BuyContractDetailSettleDTO.class, settlements);
                BigDecimal subTotal = settles.stream().filter(e -> !ObjectUtils.isEmpty(e.getAmountSettlement()))
                        .map(BuyContractDetailSettleDTO::getAmountSettlement)
                        .reduce(BigDecimal.ZERO, BigDecimal::add).setScale(2, BigDecimal.ROUND_HALF_UP);
                saleContractBasicMapper.updateById(contractBasic.setAmountSettlementTotal(subTotal));
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
    }
}
