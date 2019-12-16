package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.dto.ConfigElectricityPriceCheckDTO;
import com.bee.platform.cloud.si.manufacture.entity.ConfigElectricityPrice;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceCheckRQ;
import com.bee.platform.cloud.si.manufacture.rq.ConfigElectricityPriceUpdateCheckRQ;

import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
public interface ConfigElectricityPriceMapper extends BaseMapper<ConfigElectricityPrice> {

    /**
     * 保存重复校验
     * @param rq
     * @return
     */
    List<ConfigElectricityPriceCheckDTO> saveRepeatCheck(ConfigElectricityPriceCheckRQ rq);


    /**
     * 修改重复校验
     * @param rq
     * @return
     */
    List<ConfigElectricityPriceCheckDTO> updateRepeatCheck(ConfigElectricityPriceUpdateCheckRQ rq);



}
