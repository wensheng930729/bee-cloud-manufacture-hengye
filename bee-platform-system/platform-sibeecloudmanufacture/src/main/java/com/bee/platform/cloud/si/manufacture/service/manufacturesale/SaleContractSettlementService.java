package com.bee.platform.cloud.si.manufacture.service.manufacturesale;

import com.bee.platform.cloud.si.manufacture.entity.SaleContractSettlement;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.rq.SaleContractSettlementRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;

/**
 * <p>
 * 销售合同结算表 服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-26
 */
public interface SaleContractSettlementService extends IService<SaleContractSettlement> {

    /**
     * 保存合同结算情况
     * @param contractSettlementRQ
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> saveContractSettleInfo(SaleContractSettlementRQ contractSettlementRQ, AuthPlatformUserInfo userInfo);

}
