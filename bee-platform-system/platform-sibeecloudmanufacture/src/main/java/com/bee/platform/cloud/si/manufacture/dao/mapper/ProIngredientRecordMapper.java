package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.dto.BlankingByMqttDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProIngredientRecord;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 配料明细统计表 Mapper 接口
 * </p>
 *
 * @author MP123
 * @since 2019-10-30
 */
public interface ProIngredientRecordMapper extends BaseMapper<ProIngredientRecord> {
    /**
     * @notes: 配料实时新增数据
     * @Author: junyang.li
     * @Date: 11:28 2019/10/31
     * @param batchId : 料批id
     * @param dto : 实时数据
     * @return: void
     */
    void updateBlankingByMqttData(@Param("batchId") Long batchId,@Param("dto") BlankingByMqttDTO dto);
}
