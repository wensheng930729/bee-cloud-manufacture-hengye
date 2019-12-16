package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.dto.BlankingByMqttDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProIngredientStatistic;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 配料明细统计表 Mapper 接口
 * </p>
 *
 * @author MP123
 * @since 2019-10-12
 */
public interface ProIngredientStatisticMapper extends BaseMapper<ProIngredientStatistic> {
    /**
     * @notes: 通过MQTT 实时更新下料数据
     * @Author: junyang.li
     * @Date: 17:02 2019/10/12
     * @param batchId : 料批id
     * @param dto : 实时数据
     * @return: void
     */
    void updateBlankingByData(@Param("batchId") Long batchId,@Param("dto") BlankingByMqttDTO dto);
}
