package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.BuyContractSettlement;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
public interface BuyContractSettlementMapper extends BaseMapper<BuyContractSettlement> {

    /**
     * 根据合同业务id查询总结算数量
     * @param contractBusinessId
     * @return
     */
    BigDecimal getSettlementVolume(@Param("contractBusinessId") String contractBusinessId);

}
