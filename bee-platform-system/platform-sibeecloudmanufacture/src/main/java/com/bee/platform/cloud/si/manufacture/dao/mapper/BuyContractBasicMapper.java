package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.BuyContractBasic;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * <p>
 * 采购合同信息表 Mapper 接口
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
public interface BuyContractBasicMapper extends BaseMapper<BuyContractBasic> {
    /**
     * 查询采购总额
     *
     * @param map
     * @return
     */
    public List<BuyPurchaseMoneyRatioDTO> getPurchaseMoney(Map<String, Object> map);


    List<UnfinishedFinanceDTO> getUnfinishedFinance(Map<String, Object> map);

    List<UnfinishedFinanceDTO> getSupplierUnfinishedFinance(Map<String, Object> map);

    public List<BuyPurchaseAmountRatioDTO> getPurchaseAmount(Map<String, Object> map);

    /**
     * 查询采购总额
     *
     * @param map
     * @return
     */
    public List<BuyPurchaseMoneyRatioDTO> getPurchasePayment(Map<String, Object> map);

    /**
     * 查询采购未完成业务的到货情况-供应商
     *
     * @param map
     * @return
     */
    public List<UnfinishedGoodsDTO> getSupplierUnfinishedGoods(HashMap<String, Object> map);

    /**
     * 查询采购未完成业务的到货情况-主料、辅料、成品
     *
     * @param map
     * @return
     */
    public List<UnfinishedGoodsDTO> getUnfinishedGoods(HashMap<String, Object> map);

    /**
     * 查询金额总览
     *
     * @param map
     * @return
     */
    public BigDecimal getDataScreen(Map<String, Object> map);

    /**
     *采购结算列表
     * @param param
     * @param pagination
     * @return
     */
    List<BuyContractSettleListDTO> getSettleListBuy(Map<String, Object> param, Pagination pagination);

    /**
     * @Description web磅单获取管理的合同号
     * @author chenxm66777123
     * @Date 2019/11/26 15:23
     * @version 1.0.0
     */
    List<WeightMachineWebRelationContractDTO> getRelationContract(Map<String, Object> param, Pagination pagination);
}
