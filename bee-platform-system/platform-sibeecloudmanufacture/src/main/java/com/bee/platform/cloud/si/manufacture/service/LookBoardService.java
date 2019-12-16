package com.bee.platform.cloud.si.manufacture.service;

import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.rq.LookBoardRQ;
import com.bee.platform.cloud.si.manufacture.rq.PurchasePaymentRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * 看板服务类
 *
 * @author dell
 */
public interface LookBoardService {

    /**
     * 采购-查看未完成业务的付款情况
     *
     * @param userInfo
     * @param type
     * @return
     */
    ResponseResult<List<UnfinishedFinanceDTO>> getBuyUnfinishedFinance(AuthPlatformUserInfo userInfo, Integer type);

    /**
     * 采购总额占比
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<BuyPurchaseMoneyRatioDTO>> getPurchaseMoneyRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 采购总数量占比
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<BuyPurchaseAmountRatioDTO>> getPurchaseAmountRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 采购总付款金额占比
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<BuyPurchaseMoneyRatioDTO>> getPurchasePaymentRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 采购合格率
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<BuyPurchasePassRatioDTO>> getPurchasePassRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 销售总额占比
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<SaleMoneyRatioDTO>> getSaleMoneyRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 销售总数量占比
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<SaleAmountRatioDTO>> getSaleAmountRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 销售回款占比
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<SaleMoneyRatioDTO>> getSaleMoneyBackRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 销售合格率
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<SalePassRatioDTO>> getSalePassRatio(PurchasePaymentRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 销售-查看未完成业务的账款情况
     *
     * @param userInfo
     * @param type
     * @return
     */
    ResponseResult<List<UnfinishedFinanceDTO>> getSaleUnfinishedFinance(AuthPlatformUserInfo userInfo, Integer type);

    /**
     * 采购-查看未完成业务的到货情况
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<UnfinishedGoodsDTO>> getBuyUnfinishedGoods(LookBoardRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 销售-查看未完成业务的到货情况
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<List<UnfinishedGoodsDTO>> getSaleUnfinishedGoods(LookBoardRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 查询BI数据总览
     *
     * @param userInfo
     * @return
     */
    ResponseResult<DataScreenDTO> getDataScreen(AuthPlatformUserInfo userInfo);


}
