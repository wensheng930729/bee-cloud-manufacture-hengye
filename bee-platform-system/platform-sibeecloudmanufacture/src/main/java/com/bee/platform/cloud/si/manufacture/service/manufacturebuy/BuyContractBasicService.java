package com.bee.platform.cloud.si.manufacture.service.manufacturebuy;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.BuyContractBasic;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.BuyContractAddRq;
import com.bee.platform.cloud.si.manufacture.rq.BuyContractListRq;
import com.bee.platform.cloud.si.manufacture.rq.BuyContractPayRq;
import com.bee.platform.cloud.si.manufacture.rq.BuyContractSettleUpdateAmountRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 * 采购合同信息表 服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
public interface BuyContractBasicService extends IService<BuyContractBasic> {

    /**
     * 合同新增
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> addContractBuy(BuyContractAddRq rq, AuthPlatformUserInfo userInfo);

    /**
     * 合同列表
     * @param rq
     * @param userInfo
     * @param pagination
     * @return
     */
    ResponseResult<BuyContractListDTO> getBuyContractList(BuyContractListRq rq, Pagination pagination, AuthPlatformUserInfo userInfo);

    /**
     * 合同详情
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    ResponseResult<BuyContractDetailTotalDTO> getBuyContractDetail(String contractBusinessId, AuthPlatformUserInfo userInfo);

    /**
     * 采购合同付款
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> payForBuyContract(BuyContractPayRq rq, AuthPlatformUserInfo userInfo);

    /**
     * 采购修改结算金额
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> updateSettleAmountBuy(BuyContractSettleUpdateAmountRq rq, AuthPlatformUserInfo userInfo);

    /**
     * 采购合同结算确认
     * @param businessId
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> confirmSettleBuyContract(String businessId, AuthPlatformUserInfo userInfo);

    /**
     * 采购合同完成
     *
     * @param businessId
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> completeBuyContract(String businessId, AuthPlatformUserInfo userInfo);

    /**
     * 合同业务id获取合同详情
     * @param businessId
     * @param
     * @return
     */
    BuyContractListContentDTO getBuyContractByBusinessId(String businessId);

    /**
     * 获取产品枚举信息
     * @param userInfo
     * @return
     */
    ResponseResult<List<ProductDTO>> getProducts(AuthPlatformUserInfo userInfo);

    /**
     * 客户 供应商下拉框
     * @param userInfo
     * @return
     */
    ResponseResult<List<SupplierOrCustomersDTO>> getSuppliersOrCustomers(AuthPlatformUserInfo userInfo,Integer type);

    /**
     * 更新合同的在途量和到货量
     */
    void updateContractVolume();

    /**
     * 采购-磅房备注
     * @param contractBusinessId
     * @return
     */
    List<PoundHouseDTO> getPoundHouseRemark(String contractBusinessId);
}
