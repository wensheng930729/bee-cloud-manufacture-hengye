package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProIngredientStatisticMapper;
import com.bee.platform.cloud.si.manufacture.dto.BlankingByMqttDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProIngredientStatistic;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProIngredientStatisticService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 * 配料明细统计表 服务实现类
 * </p>
 *
 * @author MP123
 * @since 2019-10-12
 */
@Service
public class ProIngredientStatisticServiceImpl extends ServiceImpl<ProIngredientStatisticMapper, ProIngredientStatistic> implements ProIngredientStatisticService {

    @Autowired
    private ProIngredientStatisticMapper proIngredientStatisticMapper;

    /**
     * @notes: 通过MQTT 实时更新下料数据
     * @Author: junyang.li
     * @Date: 17:02 2019/10/12
     * @param batchId : 料批id
     * @param list : 实时数据
     * @return: void
     */
    @Override
    public void updateBlankingByMqttData(Long batchId, List<BlankingByMqttDTO> list) {
        if(!CollectionUtils.isEmpty(list)){
            list.forEach(obj->proIngredientStatisticMapper.updateBlankingByData(batchId,obj));
        }
    }
}
