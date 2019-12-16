package com.bee.platform.cloud.si.manufacture.service.manufactureproduce;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.dto.BlankingByMqttDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProIngredientStatistic;

import java.util.List;

/**
 * <p>
 * 配料明细统计表 服务类
 * </p>
 *
 * @author MP123
 * @since 2019-10-12
 */
public interface ProIngredientStatisticService extends IService<ProIngredientStatistic> {

    /**
     * @notes: 通过MQTT 实时更新下料数据
     * @Author: junyang.li
     * @Date: 17:02 2019/10/12
     * @param batchId : 料批id
     * @param list : 实时数据
     * @return: void
     */
    void updateBlankingByMqttData(Long batchId, List<BlankingByMqttDTO> list);
}
