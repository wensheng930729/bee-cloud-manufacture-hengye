package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.dto.ProArtificialFeedDTO;
import com.bee.platform.cloud.si.manufacture.dto.ProFurnaceProductionDetailDTO;
import com.bee.platform.cloud.si.manufacture.entity.ProArtificialFeed;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.cloud.si.manufacture.rq.ProStatisticRQ;

import java.util.List;

/**
 * <p>
 * 人工补料表 Mapper 接口
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-25
 */
public interface ProArtificialFeedMapper extends BaseMapper<ProArtificialFeed> {

    /**
     * @descriptin 查询人工补料中的原料总耗
     * @author xin.huang
     * @param rq
     * @date 2019/11/6
     * @return
     */
    List<ProArtificialFeedDTO> findConsume(ProStatisticRQ rq);

    /**
     * @descriptin 查询人工补料中各原料消耗
     * @author xin.huang
     * @param rq
     * @date 2019/11/6
     * @return
     */
    List<ProArtificialFeedDTO> findMainOrAuxiliary(ProStatisticRQ rq);
}
