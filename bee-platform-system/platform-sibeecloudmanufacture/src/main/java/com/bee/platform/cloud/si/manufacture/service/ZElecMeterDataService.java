package com.bee.platform.cloud.si.manufacture.service;

import java.util.List;

import com.bee.platform.cloud.si.manufacture.entity.ZElecMeterData;
import com.bee.platform.cloud.si.manufacture.entity.ZElecMeterDataVO;



public interface ZElecMeterDataService {


    /**
     * 按时间查询所有电表数据
     * @param zElecMeterDataVO
     * @return
     */
    List<ZElecMeterData> findByDuration(ZElecMeterDataVO zElecMeterDataVO);

}
