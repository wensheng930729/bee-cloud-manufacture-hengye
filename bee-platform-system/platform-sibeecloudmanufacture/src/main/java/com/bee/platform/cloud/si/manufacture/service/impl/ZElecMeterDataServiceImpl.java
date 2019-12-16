package com.bee.platform.cloud.si.manufacture.service.impl;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.bee.platform.cloud.si.manufacture.entity.ESZElecMeterData;
import com.bee.platform.cloud.si.manufacture.entity.ZElecMeterData;
import com.bee.platform.cloud.si.manufacture.entity.ZElecMeterDataVO;
import com.bee.platform.cloud.si.manufacture.service.ZElecMeterDataService;
import com.bee.platform.cloud.si.manufacture.utils.ListUtils;

@Service
@Transactional(rollbackFor = Exception.class)
public class ZElecMeterDataServiceImpl extends BaseEsServiceImpl<ESZElecMeterData, ZElecMeterData> implements ZElecMeterDataService {
    private static final Logger log = LoggerFactory.getLogger(ZElecMeterDataServiceImpl.class);


    /**
     * 按时间查询所有电表数据
     *
     * @param zElecMeterDataVO
     * @return
     */
    @Override
    public List<ZElecMeterData> findByDuration(ZElecMeterDataVO zElecMeterDataVO) {

        Long startTime = zElecMeterDataVO.getStartTime() / 1000;
        Long endTime = zElecMeterDataVO.getEndTime() / 1000;
        EsCommonCondition condition = EsCommonCondition.builder().factoryId(zElecMeterDataVO.getFactoryId())
            .deviceId(zElecMeterDataVO.getDeviceId()).startTime(startTime).endTime(endTime)
            .timeField("meterReadTime").sortClause("meterReadTime.desc").pageFlag(false).build();
        List<ZElecMeterData> zElecMeterDataList = getPageDataWithFactoryAndDeviceByRange(condition);
        if (ListUtils.isEmpty(zElecMeterDataList)) {
            return null;
        }
        return zElecMeterDataList;
    }


}
