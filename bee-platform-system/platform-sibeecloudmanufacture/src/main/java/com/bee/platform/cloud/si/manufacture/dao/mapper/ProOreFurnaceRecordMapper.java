package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.ProOreFurnaceRecord;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.rq.ProOreFurnaceRecordQueryRQ;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 矿热炉记录 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-26
 */
public interface ProOreFurnaceRecordMapper extends BaseMapper<ProOreFurnaceRecord> {

    List<ProOreFurnaceRecord> findRecordsByCondition(ProOreFurnaceRecordQueryRQ rq);

}
