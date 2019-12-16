package com.bee.platform.cloud.si.manufacture.service.manufactureproduce.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.cloud.si.manufacture.dao.mapper.ProIngredientRecordMapper;
import com.bee.platform.cloud.si.manufacture.dto.BlankingByMqttDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProIngredientRecord;
import com.bee.platform.cloud.si.manufacture.service.manufactureproduce.ProIngredientRecordService;
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
 * @since 2019-10-30
 */
@Service
public class ProIngredientRecordServiceImpl extends ServiceImpl<ProIngredientRecordMapper, ProIngredientRecord>
        implements ProIngredientRecordService {

    @Autowired
    private ProIngredientRecordMapper proIngredientRecordMapper;

    /**
     * @notes: 配料实时新增数据
     * @Author: junyang.li
     * @Date: 11:28 2019/10/31
     * @param batchId :
     * @param list :
     * @return: void
     */
    @Override
    public void updateBlankingByMqttData(Long batchId, List<BlankingByMqttDTO> list) {
        if(batchId == null || CollectionUtils.isEmpty(list)){
            return;
        }
        list.forEach(obj->proIngredientRecordMapper.updateBlankingByMqttData(batchId,obj));

    }
}
