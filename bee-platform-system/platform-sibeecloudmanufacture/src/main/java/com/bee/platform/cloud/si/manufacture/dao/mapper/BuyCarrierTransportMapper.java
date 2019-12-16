package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.BuyTransportReportDTO;
import com.bee.platform.cloud.si.manufacture.entity.BuyCarrierTransport;
import com.bee.platform.cloud.si.manufacture.rq.BuyCarrierTransportSearchRQ;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 承运方运输表(采购) Mapper 接口
 * </p>
 *
 * @author qhwang
 * @since 2019-09-23
 */
public interface BuyCarrierTransportMapper extends BaseMapper<BuyCarrierTransport> {

    /**
     * 根据承运商名称查询承运商正在运输的运输段信息
     * @param carrierName
     * @param page
     * @return
     */
    List<BuyCarrierTransport> getTransportInfoByCarrier(@Param("carrierName") String carrierName, Pagination page);

    /**
     * 根据条件查采购运输台账
     * @param param
     * @param pagination
     * @return
     */
    List<BuyTransportReportDTO> getBuyLogisticsReportForm(Map<String, Object> param, Pagination pagination);

}
