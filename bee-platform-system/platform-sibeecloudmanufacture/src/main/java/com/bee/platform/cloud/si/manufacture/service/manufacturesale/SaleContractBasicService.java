package com.bee.platform.cloud.si.manufacture.service.manufacturesale;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.SaleContractDetailTotalDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleContractListContentDTO;
import com.bee.platform.cloud.si.manufacture.dto.SaleContractListDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleContractBasic;
import com.bee.platform.cloud.si.manufacture.rq.*;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

/**
 * <p>
 * 销售合同信息表 服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-26
 */
public interface SaleContractBasicService extends IService<SaleContractBasic> {

    /**
     * 合同新增
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> addContractBuy(SaleContractAddRq rq, AuthPlatformUserInfo userInfo);

    /**
     * 合同列表
     * @param rq
     * @param userInfo
     * @param pagination
     * @return
     */
    ResponseResult<SaleContractListDTO> getSaleContractList(BuyContractListRq rq, Pagination pagination, AuthPlatformUserInfo userInfo);

    /**
     * 合同详情
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    ResponseResult<SaleContractDetailTotalDTO> getSaleContractDetail(String contractBusinessId, AuthPlatformUserInfo userInfo);

    /**
     * 销售合同收款
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> receiveForSaleContract(SaleContractReceiveRq rq, AuthPlatformUserInfo userInfo);

    /**
     * 销售修改结算金额
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> updateSettleAmountSaleContract(BuyContractSettleUpdateAmountRq rq, AuthPlatformUserInfo userInfo);

    /**
     * 销售合同结算确认
     * @param businessId
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> confirmSettleSaleContract(String businessId, AuthPlatformUserInfo userInfo);

    /**
     * 销售合同完成
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> completeSaleContract(SaleContractCompleteRq rq, AuthPlatformUserInfo userInfo);

    /**
     * 合同业务id获取合同详情
     * @param businessId
     * @param
     * @return
     */
    SaleContractListContentDTO getSaleContractByBusinessId(String businessId);

    /**
     * 更新合同的在途量和到货量
     */
    void updateContractVolume();


}
