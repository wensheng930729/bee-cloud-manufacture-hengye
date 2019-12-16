package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.cloud.si.manufacture.dto.SaleTransportReportDTO;
import com.bee.platform.cloud.si.manufacture.entity.SaleCarrierTransport;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.rq.SaleCarrierTransportSearchRQ;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 运输段承运方表(销售) Mapper 接口
 * </p>
 *
 * @author qhwang
 * @since 2019-09-26
 */
public interface SaleCarrierTransportMapper extends BaseMapper<SaleCarrierTransport> {

    /**
     * 根据条件查销售运输台账
     * @param param
     * @param pagination
     * @return
     */
    List<SaleTransportReportDTO> getSaleLogisticsReportForm(Map<String, Object> param, Pagination pagination);

}
