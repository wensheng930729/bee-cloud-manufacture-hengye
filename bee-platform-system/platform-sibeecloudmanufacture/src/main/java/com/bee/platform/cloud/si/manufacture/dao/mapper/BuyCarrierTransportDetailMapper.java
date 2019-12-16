package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.entity.BuyCarrierTransportDetail;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 承运方运输详情表(采购) Mapper 接口
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
public interface BuyCarrierTransportDetailMapper extends BaseMapper<BuyCarrierTransportDetail> {

    /**
     * 查询批次中到厂的车辆信息
     * @param contractBusinessId
     * @param batchId
     * @return
     */
    List<BuyCarrierTransportDetail> getToFactoryCarInfo(@Param("contractBusinessId") String contractBusinessId,
                                                        @Param("batchId") String batchId);

    /**
     * 查询阶段下的运量总和
     * @param transportSectionId
     * @return
     */
    BigDecimal selectVolumeByTransportSection(@Param("transportSectionId") String transportSectionId);

}
