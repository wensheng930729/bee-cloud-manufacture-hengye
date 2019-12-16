package com.bee.platform.cloud.si.manufacture.service.manufacturebuy;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractListDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractSettleListDTO;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractSettlePopupDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuyContractSettlement;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.BuySettleListRq;
import com.bee.platform.cloud.si.manufacture.rq.BuySettlePopupWindowSaveRq;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.cloud.si.manufacture.dto.BuyContractSettleInfoDTO;
import com.bee.platform.cloud.si.manufacture.rq.BuyContractSettlementRQ;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
public interface BuyContractSettlementService extends IService<BuyContractSettlement> {

    /**
     * 采购合同结算列表
     * @param settleStatus
     * @param userInfo
     * @param pagination
     * @return
     */
    ResponseResult<BuyContractListDTO> getSettleListBuyContract(Integer settleStatus, AuthPlatformUserInfo userInfo, Pagination pagination);

    /** 根据合同业务id查询合同结算详情
     * @param contractBusinessId
     * @return
     */
    ResponseResult<BuyContractSettleInfoDTO> getContractSettleInfo(String contractBusinessId);

    /**
     * 保存合同结算情况
     * @param contractSettlementRQ
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> saveContractSettleInfo(BuyContractSettlementRQ contractSettlementRQ, AuthPlatformUserInfo userInfo);

    /**
     * 确认合同重量结算
     * @param contractSettlementBusinessId
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> sureSettleWeight(String contractSettlementBusinessId, AuthPlatformUserInfo userInfo);

    /**
     * 合同确认结算
     * @param contractBusinessId
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> sureContractSettle(String contractBusinessId, AuthPlatformUserInfo userInfo);

    /**
     * 采购结算列表
     * @param userInfo
     * @param rq
     * @param pagination
     * @return
     */
    ResponseResult<List<BuyContractSettleListDTO>> getSettleListBuy(AuthPlatformUserInfo userInfo, BuySettleListRq rq, Pagination pagination);

    /**
     * 采购结算弹窗
     * @param userInfo
     * @param contractBusinessId
     * @param settleStatus
     * @return
     */
    ResponseResult<BuyContractSettlePopupDTO> getSettleBuyPopupWindow(AuthPlatformUserInfo userInfo, String contractBusinessId,Integer settleStatus,Integer settleId);

    /**
     * 采购结算弹窗结算
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<ResCodeEnum> saveSettleBuyPopupWindow(AuthPlatformUserInfo userInfo, BuySettlePopupWindowSaveRq rq);
}
