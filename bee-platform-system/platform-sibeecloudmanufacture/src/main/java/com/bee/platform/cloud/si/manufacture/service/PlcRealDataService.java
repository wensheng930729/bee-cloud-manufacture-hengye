package com.bee.platform.cloud.si.manufacture.service;

import com.bee.platform.cloud.si.manufacture.dto.GatewayRealDataDTO;
import com.bee.platform.cloud.si.manufacture.entity.PlcRealData;
import com.baomidou.mybatisplus.service.IService;

import java.math.BigDecimal;
import java.util.Map;

/**
 * <p>
 * plc硬件通过mqtt传输的实时数据 服务类
 * </p>
 *
 * @author MP123
 * @since 2019-10-10
 */
public interface PlcRealDataService extends IService<PlcRealData> {
    /**
     * @notes: 处理plc采集的数据
     * @Author: junyang.li
     * @Date: 18:52 2019/10/10
     * @param dto : 采集数据
     * @return: void
     */
    void  tacklePlcData(GatewayRealDataDTO dto);
    /**
     * @notes: 配料统计
     * @Author: junyang.li
     * @Date: 16:02 2019/10/12
     * @param plcId :
     * @return: java.util.Map<java.lang.Integer,java.math.BigDecimal>
     */
    Map<Integer, BigDecimal> getPlcCountData(int plcId);
}
